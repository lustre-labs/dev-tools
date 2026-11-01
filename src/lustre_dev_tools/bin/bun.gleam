// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/bit_array
import gleam/crypto
import gleam/dynamic/decode
import gleam/erlang/application
import gleam/erlang/process.{type Subject}
import gleam/http
import gleam/http/request.{Request}
import gleam/httpc
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import lustre_dev_tools/cli
import lustre_dev_tools/error.{type Error}
import lustre_dev_tools/port
import lustre_dev_tools/project.{type Project}
import lustre_dev_tools/system
import simplifile
import tom

//

/// Download bun cli executable, verify integrity and copy to lustre bin path.
/// Returns absolute path to executable.
///
pub fn download(
  project: Project,
  quiet quiet: Bool,
  timeout_ms timeout_ms: Int,
) -> Result(String, Error) {
  // 1. First detect what release we need to download based on the user's system
  //    information. If this fails it's because Bun doesn't support the user's
  //    system.
  use name <- result.try(detect_platform())

  // 2. Grab the appropriate release from GitHub using Erlang's HTTP client.
  let req =
    Request(
      method: http.Get,
      headers: [],
      body: <<>>,
      scheme: http.Https,
      host: "github.com",
      port: option.None,
      path: "/oven-sh/bun/releases/download/bun-v"
        <> version
        <> "/"
        <> name
        <> ".zip",
      query: option.None,
    )

  cli.log("Downloading Bun v" <> version, quiet)

  use res <- result.try(
    httpc.configure()
    // GitHub will redirect us to a CDN for the download, so we need to make sure
    // httpc is configured to follow redirects.
    |> httpc.timeout(timeout_ms)
    |> httpc.follow_redirects(True)
    |> httpc.dispatch_bits(req)
    |> result.map_error(error.CouldNotDownloadBunBinary),
  )

  cli.log("Verifying download hash", quiet)

  // 3. Before we do anything else, we need to verify the SHA-256 hash of the
  //    downloaded archive. This is to ensure that the archive hasn't been
  //    tampered with or corrupted during the download.
  use _ <- result.try(verify_integrity(res.body, name))

  // 4. Extract the downloaded archive to the given path, this happens without
  //    loading the binary into our program's memory: we just get back the path
  //    to the Bun binary itself.
  use path <- result.try(
    extract(res.body, to: project.bin)
    |> result.replace_error(error.CouldNotExtractBunArchive(
      os: system.detect_os(),
      arch: system.detect_arch(),
      version:,
    )),
  )

  // 5. We downloaded an executable, so we need to make it executable! This can
  //    fail if the user doesn't have permission to change this.
  use _ <- result.try(
    simplifile.set_permissions_octal(path, 0o755)
    |> result.map_error(error.CouldNotWriteFile(path:, reason: _)),
  )

  // 6. This shouldn't be necessary because we already verified the archive hash,
  //    but as a final step we run `bun --version` and make sure it matches the
  //    version we support.
  use _ <- result.try(verify_version(path))

  cli.success("Bun v" <> version <> " is ready to go!", quiet)

  Ok(path)
}

///
///
pub fn watch(
  project: Project,
  directories: List(String),
  on_change: fn(String, String) -> Nil,
) -> Result(Subject(port.Message), Error) {
  use path <- result.try(locate_bun(project, False))

  let assert Ok(priv) = application.priv_directory("lustre_dev_tools")
  let watcher = filepath.join(priv, "bun-watcher.js")

  // 2. Start an Erlang port to run the Bun binary with our file watching script.
  use port <- result.try(
    port.start(
      cmd: path,
      with: [watcher],
      on_data: fn(event) {
        let decoder = {
          use root <- decode.field("in", decode.string)
          use file <- decode.field("name", decode.string)

          decode.success(#(root, file))
        }

        case decode.run(event, decoder) {
          Ok(#(root, file)) -> on_change(root, file)
          Error(_) -> Nil
        }
      },
      on_unknown: fn() { Nil },
    )
    |> result.replace_error(error.CouldNotStartFileWatcher(
      watcher: path,
      os: system.detect_os(),
      arch: system.detect_arch(),
    )),
  )

  // 3.
  list.each(directories, fn(directory) {
    port.send(port, {
      json.object([
        #("$", json.string("start")),
        #("path", json.string(directory)),
      ])
    })
  })

  Ok(port)
}

///
///
pub fn build(
  project: Project,
  entries entries: List(String),
  outdir outdir: String,
  minify minify: Bool,
  quiet quiet: Bool,
) -> Result(Nil, Error) {
  use cmd <- result.try(locate_bun(project, quiet))

  let args =
    list.flatten([
      ["build"],
      entries,
      maybe(minify, ["--minify"], []),
      maybe(list.length(entries) > 1, ["--splitting"], []),
      ["--outdir", outdir],
    ])

  cli.log("Creating JavaScript bundle", quiet)

  use _ <- result.try(
    system.run(cmd, args, [])
    |> result.map_error(error.FailedToBuildProject),
  )

  cli.success("Bundle successfully built.", quiet)

  Ok(Nil)
}

// UTILS -----------------------------------------------------------------------

///
///
fn locate_bun(project: Project, quiet: Bool) -> Result(String, Error) {
  case tom.get_string(project.options, ["bin", "bun"]) {
    Ok("system") ->
      system.find("bun")
      |> result.replace_error(error.CouldNotLocateBunBinary(path: "$PATH/bun"))
      |> result.map(fn(path) {
        cli.log("Using system Bun installation", quiet)
        path
      })

    Ok(path) ->
      case simplifile.is_file(path) {
        Ok(True) -> {
          cli.log("Using local Bun installation", quiet)
          Ok(path)
        }

        Ok(False) | Error(_) -> Error(error.CouldNotLocateBunBinary(path:))
      }

    Error(_) -> {
      use name <- result.try(detect_platform())
      let path =
        project.bin
        |> filepath.join(name)
        |> filepath.join(system.executable_name("bun"))

      // Detect if we're already downloaded Bun because of an earlier command.
      // If not, we automatically download it now.
      let timeout =
        tom.get_int(project.options, ["bin", "timeout"])
        |> result.map(fn(seconds) { seconds * 1000 })
        |> result.unwrap(60_000)

      use path <- result.try(case simplifile.is_file(path) {
        Ok(True) -> {
          case verify_version(path) {
            Ok(_) -> Ok(path)
            Error(_) -> download(project, quiet, timeout)
          }
        }
        _ -> download(project, quiet, timeout)
      })

      Ok(path)
    }
  }
}

/// Detect the user's operating system and CPU architecture, and then resolve the
/// name of the Bun archive we need to download.
///
fn detect_platform() -> Result(String, Error) {
  let os = system.detect_os()
  let arch = system.detect_arch()

  resolve(os, arch)
  |> result.replace_error(error.UnsupportedPlatform(os:, arch:))
}

/// Verifies the integrity of the downloaded archive by comparing its SHA-256
/// hash against the hashes provided by Bun:
///
///   https://github.com/oven-sh/bun/releases/download/bun-v1.3.13/SHASUMS256.txt
///
fn verify_integrity(archive: BitArray, name: String) -> Result(Nil, Error) {
  let assert Ok(expected) = list.key_find(hashes, name)
  let hash = crypto.hash(crypto.Sha256, archive)
  let actual = bit_array.base16_encode(hash) |> string.lowercase

  case actual == expected {
    True -> Ok(Nil)
    False -> Error(error.CouldNotVerifyBunHash(expected:, actual:))
  }
}

/// Resolve the full name of the Bun archive we need to download based on the
/// system's OS and CPU architecture. This will get us something like
/// `"bun-linux-x64-baseline"` or `"bun-darwin-aarch64"`.
///
fn resolve(os: String, arch: String) -> Result(String, Nil) {
  let is_alpine = system.is_alpine()
  let baseline = requires_baseline(os)

  case os, arch {
    "darwin", "aarch64" | "darwin", "arm64" -> Ok("bun-darwin-aarch64")
    "darwin", "x64" | "darwin", "x86_64" -> Ok("bun-darwin-x64")
    "linux", "aarch64" | "linux", "arm64" if is_alpine ->
      Ok("bun-linux-aarch64-musl")
    "linux", "aarch64" | "linux", "arm64" -> Ok("bun-linux-aarch64")
    "linux", "x64" | "linux", "x86_64" ->
      case baseline, is_alpine {
        True, True -> Ok("bun-linux-x64-musl-baseline")
        True, False -> Ok("bun-linux-x64-baseline")
        False, True -> Ok("bun-linux-x64-musl")
        False, False -> Ok("bun-linux-x64")
      }
    "win32", "x64"
    | "windows", "x64"
    | "win32", "x86_64"
    | "windows", "x86_64"
      if baseline
    -> Ok("bun-windows-x64-baseline")
    "win32", "x64"
    | "windows", "x64"
    | "win32", "x86_64"
    | "windows", "x86_64"
    -> Ok("bun-windows-x64")
    _, _ -> Error(Nil)
  }
}

fn requires_baseline(os: String) -> Bool {
  case os {
    "darwin" -> {
      let sh = system.find("sh") |> result.unwrap("/bin/sh")
      let args = ["-c", "sysctl -a | grep -i machdep.cpu.leaf7_features"]

      case system.run(sh, args, []) {
        Ok(output) -> !string.contains(output, "AVX2")
        Error(_) -> False
      }
    }

    "linux" -> {
      let sh = system.find("sh") |> result.unwrap("/bin/sh")

      case system.run(sh, ["-c", "cat /proc/cpuinfo | grep avx2"], []) {
        Ok(output) -> output == ""
        Error(_) -> True
      }
    }

    "win32" | "windows" -> {
      let powershell =
        system.find("powershell")
        |> result.unwrap(
          "c:/WINDOWS/System32/WindowsPowerShell/v1.0/powershell.exe",
        )

      let command =
        "(Add-Type -MemberDefinition '[DllImport(\"kernel32.dll\")] public static extern bool IsProcessorFeaturePresent(int ProcessorFeature);' -Name 'Kernel32' -Namespace 'Win32' -PassThru)::IsProcessorFeaturePresent(40)"

      case system.run(powershell, ["-Command", command], []) {
        Ok(output) -> string.trim(output) != "True"
        Error(_) -> True
      }
    }

    _ -> False
  }
}

/// Extracts the downloaded Bun archive to the specified path. This interacts
/// with the file system directly and the result returned is the path to the
/// binary itself.
///
@external(erlang, "bun_ffi", "extract")
fn extract(archive: BitArray, to path: String) -> Result(String, Nil)

/// Verifies that the version of the Bun binary we have matches the version we
/// support. This is particularly useful in cases where the user provides a path
/// to their own binary.
///
fn verify_version(path: String) -> Result(Nil, Error) {
  use output <- result.try(
    system.run(path, ["--version"], [])
    |> result.replace_error(error.CouldNotLocateBunBinary(path:)),
  )
  let actual = string.trim(output)

  case actual == version {
    True -> Ok(Nil)
    False ->
      Error(error.UnsupportedBunVersion(path:, expected: version, actual:))
  }
}

///
fn maybe(when condition: Bool, then true: a, otherwise false: a) {
  case condition {
    True -> true
    False -> false
  }
}

// CONSTANTS -------------------------------------------------------------------

const version = "1.3.13"

const hashes = [
  #(
    "bun-darwin-aarch64",
    "5467e3f65dba526b9fea98f0cce04efafc0c63e169733ec27b876a3ad32da190",
  ),
  #(
    "bun-darwin-x64",
    "e5a6c8b64f419925232d111ecb13e25f0abf55e54f792341f987623fd0778009",
  ),
  #(
    "bun-darwin-x64-baseline",
    "a98ba6a480f22fda9b343626b906a4e26aa53618bf85d2bc5928ecf2ba45f0ed",
  ),
  #(
    "bun-linux-aarch64-musl",
    "5385e978107ce4934298d8d6afe9bfbb898683f6cc23e6753a0da60bc60c5b81",
  ),
  #(
    "bun-linux-aarch64",
    "70bae41b3908b0a120e1e58c5c8af30e74afae3b8d11b0d3fdd8e787ddfb4b22",
  ),
  #(
    "bun-linux-x64-baseline",
    "9d8a24292a7068090205daac0a5a223f5f69736f5287e37bf88d3b4031edc750",
  ),
  #(
    "bun-linux-x64-musl-baseline",
    "88ca7c7ad235b498f549eea2f770f434e9f0f5e9ba95168a2d3a1f235184c394",
  ),
  #(
    "bun-linux-x64-musl",
    "5b91a48f0b00df9fd2da8bff1a795d2659d842da966432969203f25da19d1c74",
  ),
  #(
    "bun-linux-x64",
    "79c0771fa8b92c33aae41e15a0e0d307ea99d0e2f00317c71c6c53237a78e25a",
  ),
  #(
    "bun-windows-x64-baseline",
    "c68c7903c1190101590cc1b2129835f47211b3b37ae87759f2b97d6534aa3ad1",
  ),
  #(
    "bun-windows-x64",
    "85b14f3e0584218e9b63407b3aa6b90c4835ec5c32435c1f12cb6fc13667c7c9",
  ),
]
