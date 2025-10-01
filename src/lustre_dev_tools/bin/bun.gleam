// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/bit_array
import gleam/bool
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

///
///
pub fn download(project: Project, quiet quiet: Bool) -> Result(Nil, Error) {
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

  Ok(Nil)
}

///
///
pub fn watch(
  project: Project,
  directories: List(String),
  on_change: fn(String, String) -> Nil,
) -> Result(Subject(port.Message), Error) {
  use path <- result.try(guard(project, False))

  let assert Ok(priv) = application.priv_directory("lustre_dev_tools")
  let watcher = filepath.join(priv, "bun-watcher.js")

  // 2. Start an Erlang port to run the Bun binary with our file watching script.
  use port <- result.try(
    port.start(cmd: path, with: [watcher], on_data: fn(event) {
      let decoder = {
        use root <- decode.field("in", decode.string)
        use file <- decode.field("name", decode.string)

        decode.success(#(root, file))
      }

      case decode.run(event, decoder) {
        Ok(#(root, file)) -> on_change(root, file)
        Error(_) -> Nil
      }
    })
    |> result.replace_error(error.CouldNotStartFileWatcher(
      os: system.detect_os(),
      arch: system.detect_arch(),
      version: version,
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
  use path <- result.try(guard(project, quiet))
  let minify = bool.guard(minify, "--minify", fn() { "" })
  let split = bool.guard(list.length(entries) > 1, "--splitting", fn() { "" })
  let flags = [minify, split, "--outdir " <> outdir]

  cli.log("Creating JavaScript bundle", quiet)

  use _ <- result.try(
    system.run(
      path
      <> " build "
      <> string.join(entries, " ")
      <> " "
      <> string.join(flags, " "),
    )
    |> result.map_error(error.FailedToBuildProject),
  )

  cli.success("Bundle successfully built.", quiet)

  Ok(Nil)
}

// UTILS -----------------------------------------------------------------------

///
///
fn guard(project: Project, quiet: Bool) -> Result(String, Error) {
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
      let path = filepath.join(project.bin, name <> "/bun")

      // Detect if we're already downloaded Bun because of an earlier command.
      // If not, we automatically download it now.
      use _ <- result.try(case simplifile.is_file(path) {
        Ok(True) ->
          case verify_version(path) {
            Ok(_) -> Ok(Nil)
            Error(_) -> download(project, quiet)
          }

        Ok(False) | Error(_) -> download(project, quiet)
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
///   https://github.com/oven-sh/bun/releases/download/bun-v1.2.17/SHASUMS256.txt
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
    "darwin", "aarch64" -> Ok("bun-darwin-aarch64")
    "darwin", "x64" -> Ok("bun-darwin-x64")
    "linux", "arm64" if is_alpine -> Ok("bun-linux-aarch64-musl")
    "linux", "arm64" -> Ok("bun-linux-aarch64")
    "linux", "aarch64" if is_alpine -> Ok("bun-linux-aarch64-musl")
    "linux", "aarch64" -> Ok("bun-linux-aarch64")
    "linux", "x64" ->
      case baseline, is_alpine {
        True, True -> Ok("bun-linux-x64-musl-baseline")
        True, False -> Ok("bun-linux-x64-baseline")
        False, True -> Ok("bun-linux-x64-musl")
        False, False -> Ok("bun-linux-x64")
      }
    "windows", "x64" if baseline -> Ok("bun-windows-x64-baseline")
    "windows", "x64" -> Ok("bun-windows-x64")
    _, _ -> Error(Nil)
  }
}

fn requires_baseline(os: String) -> Bool {
  case os {
    "linux" ->
      case system.run("cat /proc/cpuinfo | grep avx2") {
        Ok(output) -> output != ""
        Error(_) -> False
      }

    "windows" -> {
      let command =
        "powershell -Command \"(Add-Type -MemberDefinition '[DllImport(\\\"kernel32.dll\\\")] public static extern bool IsProcessorFeaturePresent(int ProcessorFeature);' -Name 'Kernel32' -Namespace 'Win32' -PassThru)::IsProcessorFeaturePresent(40)\""

      case system.run(command) {
        Ok(output) -> string.trim(output) != "True"
        Error(_) -> False
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
    system.run(path <> " --version")
    |> result.replace_error(error.CouldNotLocateBunBinary(path:)),
  )
  let actual = string.trim(output)

  case actual == version {
    True -> Ok(Nil)
    False ->
      Error(error.UnsupportedBunVersion(path:, expected: version, actual:))
  }
}

// CONSTANTS -------------------------------------------------------------------

const version = "1.2.22"

const hashes = [
  #(
    "bun-darwin-aarch64",
    "eb8c7e09cbea572414a0a367848e1acbf05294a946a594405a014b1fb3b3fc76",
  ),
  #(
    "bun-darwin-x64",
    "a7484721a7ead45887c812e760b124047e663173cf2a3ba7c5aa1992cb22cd3e",
  ),
  #(
    "bun-linux-aarch64-musl",
    "e85feb78bbff552ff6dcde0d99a7618f20df4544efd6248d9e97939709bc9406",
  ),
  #(
    "bun-linux-aarch64",
    "88c54cd66169aeb5ff31bc0c9d74a8017c7e6965597472ff49ecc355acb3a884",
  ),
  #(
    "bun-linux-x64-baseline",
    "f753e8d9668078ab0f598ee26a9ac5acbbb822e057459dd50c191b86524d98e8",
  ),
  #(
    "bun-linux-x64-musl-baseline",
    "4048e872b16fb3a296e89268769d3e41152f477b6f203eff58c672f69ed9f570",
  ),
  #(
    "bun-linux-x64-musl",
    "dde5bd79f0e130cb9bf17f55ba1825e98a77f71ef78c575d8ca2ccae5431f47e",
  ),
  #(
    "bun-linux-x64",
    "4c446af1a01d7b40e1e11baebc352f9b2bfd12887e51b97dd3b59879cee2743a",
  ),
  #(
    "bun-windows-x64-baseline",
    "c44de73dc21c7140a8e15883c28abed60612196faaec9a60c275534280a49f59",
  ),
  #(
    "bun-windows-x64",
    "3a28c685b47a159c5707d150accb5b4903c30f1e7b4dd01bb311d4112bdeb452",
  ),
]
