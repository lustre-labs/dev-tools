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
import lustre_dev_tools/error.{type Error}
import lustre_dev_tools/port
import lustre_dev_tools/project.{type Project}
import lustre_dev_tools/system
import simplifile

//

///
///
pub fn download(config: Project) -> Result(Nil, Error) {
  // 1. First detect what release we need to download based on the user's system
  //    information. If this fails it's because Bun doesn't support the user's
  //    system.
  use name <- result.try(detect())

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

  use res <- result.try(
    httpc.configure()
    // GitHub will redirect us to a CDN for the download, so we need to make sure
    // httpc is configured to follow redirects.
    |> httpc.follow_redirects(True)
    |> httpc.dispatch_bits(req)
    |> result.map_error(error.CouldNotDownloadBunBinary),
  )

  // 3. Before we do anything else, we need to verify the SHA-256 hash of the
  //    downloaded archive. This is to ensure that the archive hasn't been
  //    tampered with or corrupted during the download.
  use _ <- result.try(verify_integrity(res.body, name))

  // 4. Extract the downloaded archive to the given path, this happens without
  //    loading the binary into our program's memory: we just get back the path
  //    to the Bun binary itself.
  use path <- result.try(
    extract(res.body, to: config.bin)
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

  Ok(Nil)
}

///
///
pub fn watch(
  config: Project,
  directories: List(String),
  on_change: fn(String, String) -> Nil,
) -> Result(Subject(port.Message), Error) {
  use name <- result.try(detect())
  let path = filepath.join(config.bin, name <> "/bun")

  // 1. Detect if we're already downloaded Bun because of an earlier command. If
  //    not, we automatically download it now.
  use _ <- result.try(case simplifile.is_file(path) {
    Ok(True) -> Ok(Nil)
    Ok(False) | Error(_) -> download(config)
  })

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
  config: Project,
  entries: List(String),
  outdir: String,
  minify: Bool,
) -> Result(Nil, Error) {
  use path <- result.try(guard(config))
  let minify = bool.guard(minify, "--minify", fn() { "" })
  let split = bool.guard(list.length(entries) > 1, "--splitting", fn() { "" })
  let flags = [minify, split, "--outdir " <> outdir]

  use _ <- result.try(
    system.run(
      path
      <> " build "
      <> string.join(entries, " ")
      <> " "
      <> string.join(flags, " "),
    )
    |> echo
    |> result.replace_error(error.Todo),
  )

  Ok(Nil)
}

///
///
pub fn install(config: Project, package: String) -> Result(Nil, Error) {
  use path <- result.try(guard(config))
  use _ <- result.try(
    system.run(path <> " add " <> package <> " --cwd " <> config.build)
    |> result.replace_error(error.Todo),
  )

  Ok(Nil)
}

///
///
pub fn run(
  config: Project,
  script: String,
  args: List(String),
) -> Result(Nil, Error) {
  use path <- result.try(guard(config))

  use _ <- result.try(
    system.run(
      echo {
        path
        <> " run "
        <> " "
        <> " --cwd "
        <> config.build
        <> " --bun "
        <> script
        <> " "
        <> string.join(args, " ")
      },
    )
    |> echo
    |> result.replace_error(error.Todo),
  )

  Ok(Nil)
}

// UTILS -----------------------------------------------------------------------

///
///
fn guard(project: Project) -> Result(String, Error) {
  use name <- result.try(detect())
  let path = filepath.join(project.bin, name <> "/bun")

  // Detect if we're already downloaded Bun because of an earlier command. If not,
  // we automatically download it now.
  use _ <- result.try(case simplifile.is_file(path) {
    Ok(True) -> Ok(Nil)
    Ok(False) | Error(_) -> download(project)
  })

  Ok(path)
}

/// Detect the user's operating system and CPU architecture, and then resolve the
/// name of the Bun archive we need to download.
///
fn detect() -> Result(String, Error) {
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
@external(erlang, "bun_ffi", "resolve")
fn resolve(os: String, arch: String) -> Result(String, Nil)

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

const version = "1.2.17"

const hashes = [
  #(
    "bun-darwin-aarch64",
    "9f55fd213f2f768d02eb5b9885aaa44b1e1307a680c18622b57095302a931af9",
  ),
  #(
    "bun-darwin-x64",
    "038023b8dbdccc93383398a0c1be2ca82716649479cfae708b533ca7a9c5d083",
  ),
  #(
    "bun-linux-aarch64-musl",
    "e85feb78bbff552ff6dcde0d99a7618f20df4544efd6248d9e97939709bc9406",
  ),
  #(
    "bun-linux-aarch64",
    "a0b996f48c977beb4e87b09a471ded7e63ee5c2fb4b72790c7ab4badbc147d6b",
  ),
  #(
    "bun-linux-x64-baseline",
    "6ea1861db6a6cd44d1c8b4bafb22006f4ae49f6a2d077623bf3f456ada026d67",
  ),
  #(
    "bun-linux-x64-musl-baseline",
    "db1264273691208b536253241cb7528a393f30139c60129947f60b0fa085bbfa",
  ),
  #(
    "bun-linux-x64-musl",
    "216778c93df72a06d214cf2e3890f99f8d18b09364f009ef54c4be000943b15d",
  ),
  #(
    "bun-linux-x64",
    "6054207074653b4dbc2320d5a61e664e4b6f42379efc18d6181bffcc07a43193",
  ),
  #(
    "bun-windows-x64-baseline",
    "803e6a7d8bed9063ef7833de036aca0d1a23e5d25cd7a8cf628cba50a3199f18",
  ),
  #(
    "bun-windows-x64",
    "869a9401e119306459a4992e4b3655484c2541c93f0ae470fc8500a82d84fd4b",
  ),
]
