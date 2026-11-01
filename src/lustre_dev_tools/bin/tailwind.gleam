// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/bit_array
import gleam/crypto
import gleam/http.{Get, Https}
import gleam/http/request.{Request}
import gleam/httpc
import gleam/list
import gleam/option.{None}
import gleam/regexp
import gleam/result
import gleam/string
import gleam_community/ansi
import lustre_dev_tools/cli
import lustre_dev_tools/error.{type Error}
import lustre_dev_tools/port
import lustre_dev_tools/project.{type Project}
import lustre_dev_tools/system
import simplifile
import tom

// TYPES -----------------------------------------------------------------------

///
///
pub type Detection {
  HasViableEntry(path: String)
  HasTailwindEntry(path: String)
  HasLegacyConfig
  Nothing
}

//

/// Download tailwindcss cli executable, verify integrity and copy to lustre bin path.
/// Returns absolute path to executable.
///
pub fn download(
  project: Project,
  generate_config generate_config: Bool,
  quiet quiet: Bool,
  timeout_ms timeout_ms: Int,
) -> Result(String, Error) {
  // 1. First detect what release we need to download based on the user's system
  //    information. If this fails it's because Tailwind doesn't support the user's
  //    system.
  use name <- result.try(detect_platform())

  // 2. Grab the appropriate release from GitHub using Erlang's HTTP client.
  let req =
    Request(
      method: Get,
      headers: [],
      body: <<>>,
      scheme: Https,
      host: "github.com",
      port: None,
      path: "/tailwindlabs/tailwindcss/releases/download/v"
        <> version
        <> "/"
        <> system.executable_name(name),
      query: None,
    )

  cli.log("Downloading TailwindCSS v" <> version, quiet)

  use res <- result.try(
    httpc.configure()
    |> httpc.timeout(timeout_ms)
    // GitHub will redirect us to a CDN for the download, so we need to make sure
    // httpc is configured to follow redirects.
    |> httpc.follow_redirects(True)
    |> httpc.dispatch_bits(req)
    |> result.map_error(error.CouldNotDownloadTailwindBinary),
  )

  cli.log("Verifying download hash", quiet)

  // 3. Before we do anything else, we need to verify the SHA-256 hash of the
  //    downloaded archive. This is to ensure that the archive hasn't been
  //    tampered with or corrupted during the download.
  use _ <- result.try(verify_integrity(res.body, name))

  let path = filepath.join(project.bin, name)

  use _ <- result.try(
    simplifile.create_directory_all(path)
    |> result.map_error(error.CouldNotWriteFile(path:, reason: _)),
  )

  let path = filepath.join(path, system.executable_name(name))

  use _ <- result.try(
    simplifile.write_bits(path, res.body)
    |> result.map_error(error.CouldNotWriteFile(path:, reason: _)),
  )

  // 5. We downloaded an executable, so we need to make it executable! This can
  //    fail if the user doesn't have permission to change this.
  use _ <- result.try(
    simplifile.set_permissions_octal(path, 0o755)
    |> result.map_error(error.CouldNotWriteFile(path:, reason: _)),
  )

  // 6. This shouldn't be necessary because we already verified the archive hash,
  //    but as a final step we run `tailwind --version` and make sure it matches
  //    the version we support.
  use _ <- result.try(verify_version(path))
  // Absolute path to exe needs to return.
  let path_to_exe = path

  cli.success("TailwindCSS v" <> version <> " is ready to go!", quiet)

  // 7.
  use detection <- result.try(case generate_config {
    True -> detect(project, project.name)
    False -> Ok(Nothing)
  })

  case detection {
    HasTailwindEntry(_) -> Ok(path_to_exe)

    HasLegacyConfig -> {
      let css = [
        "@import \"tailwindcss\";",
        "@config \"../tailwind.config.js\";",
        "",
      ]

      use _ <- result.try(
        simplifile.write(path, string.join(css, "\n"))
        |> result.map_error(error.CouldNotWriteFile(path:, reason: _)),
      )

      cli.success("Tailwind config generated", quiet)

      Ok(path_to_exe)
    }

    HasViableEntry(path:) -> {
      use css <- result.try(
        simplifile.read(path)
        |> result.map_error(error.CouldNotReadFile(path:, reason: _)),
      )

      let css = ["@import \"tailwindcss\";", "", css]

      use _ <- result.try(
        simplifile.write(path, string.join(css, "\n"))
        |> result.map_error(error.CouldNotWriteFile(path:, reason: _)),
      )

      cli.success("Tailwind config generated", quiet)

      Ok(path_to_exe)
    }

    Nothing if generate_config -> {
      let config_path = filepath.join(project.src, project.name <> ".css")
      let css = ["@import \"tailwindcss\";", ""]

      use _ <- result.try(
        simplifile.write(config_path, string.join(css, "\n"))
        |> result.map_error(error.CouldNotWriteFile(path:, reason: _)),
      )

      cli.success("Tailwind config generated", quiet)

      Ok(path_to_exe)
    }

    Nothing -> Ok(path_to_exe)
  }
}

///
///
pub fn detect(project: Project, entry: String) -> Result(Detection, Error) {
  // 1. Check if the user has a CSS file that is - or could be - the entry for
  //    Tailwind.
  let tailwind_config = {
    let src_path = filepath.join(project.src, entry <> ".css")
    let dev_path = filepath.join(project.dev, entry <> ".css")

    case simplifile.is_file(src_path) {
      Ok(True) -> Ok(src_path)
      Ok(False) | Error(_) ->
        case simplifile.is_file(dev_path) {
          Ok(True) -> Ok(dev_path)
          Ok(False) | Error(_) -> Error(Nil)
        }
    }
  }

  // 2. Detect if they have a legacy Tailwind v3 config file, which is
  //    deprecated but still supported in v4.
  let legacy_config = filepath.join(project.root, "tailwind.config.js")
  let has_legacy_config =
    legacy_config
    |> simplifile.is_file
    |> result.unwrap(False)

  case tailwind_config {
    Ok(path) -> {
      use css <- result.try(
        simplifile.read(path)
        |> result.map_error(error.CouldNotReadFile(path, _)),
      )

      let assert Ok(re) = regexp.from_string("@import\\s+['\"]tailwindcss")
      case regexp.check(re, css) {
        True -> Ok(HasTailwindEntry(path:))
        False -> Ok(HasViableEntry(path:))
      }
    }
    Error(_) if has_legacy_config -> Ok(HasLegacyConfig)
    Error(_) -> Ok(Nothing)
  }
}

pub fn build(
  project project: Project,
  input entry: String,
  outdir out: String,
  minify minify: Bool,
  quiet quiet: Bool,
) -> Result(Nil, Error) {
  use cmd <- result.try(locate_tailwind(project, quiet:))

  let name = filepath.base_name(entry)
  let output = filepath.join(project.root, out) |> filepath.join(name)

  let args =
    list.flatten([
      ["-i", entry],
      ["-o", output],
      case minify {
        True -> ["--minify"]
        False -> []
      },
    ])

  cli.log("Building Tailwind stylesheet", quiet)

  use _ <- result.try(
    system.run(cmd, args, [])
    |> result.map_error(error.FailedToBuildProject),
  )

  cli.success("Stylesheet successfully built.", quiet)

  Ok(Nil)
}

pub fn watch(
  project project: Project,
  input entry: String,
  outdir out: String,
  quiet quiet: Bool,
  on_change handle_change: fn() -> Nil,
) -> Result(Nil, Error) {
  use path <- result.try(locate_tailwind(project, quiet:))
  let name = filepath.base_name(entry)
  let output = filepath.join(project.root, out) |> filepath.join(name)
  let flags = ["-i", entry, "-o", output, "-w"]

  use _ <- result.try(
    port.start(
      cmd: path,
      with: flags,
      on_data: fn(_) { Nil },
      // Our port abstraction is really meant for port programs we control and
      // can send JSON in/out. Of course tailwind is not one of these so all the
      // "messages" it sends will be handled in the `on_unknown` callback.
      //
      // Fortunately (?) Tailwind's output only really tells is it rebuilt and
      // nothing else so there's no data we'd need to parse out anyway.
      on_unknown: fn() {
        // Check that the output file has actually been modified before resending assets
        let file_info = simplifile.file_info(output)
        case file_info {
          Ok(info) -> {
            case info.mtime_seconds > info.atime_seconds {
              True -> handle_change()
              False -> Nil
            }
          }
          Error(_e) -> {
            cli.log("Failed to locate output css file", quiet)
          }
        }
      },
    )
    |> result.replace_error(error.CouldNotStartFileWatcher(
      watcher: path,
      os: system.detect_os(),
      arch: system.detect_arch(),
    )),
  )

  Ok(Nil)
}

// UTILS -----------------------------------------------------------------------

///
///
fn locate_tailwind(
  project: Project,
  quiet quiet: Bool,
) -> Result(String, Error) {
  case tom.get_string(project.options, ["bin", "tailwindcss"]) {
    Ok("system") ->
      system.find("tailwindcss")
      |> result.replace_error(error.CouldNotLocateTailwindBinary(
        path: "$PATH/tailwindcss",
      ))
      |> result.map(fn(path) {
        cli.log("Using system Tailwind installation", quiet)
        path
      })

    Ok(path) ->
      case simplifile.is_file(path) {
        Ok(True) -> {
          cli.log("Using local Tailwind installation", quiet)
          Ok(path)
        }

        Ok(False) | Error(_) -> Error(error.CouldNotLocateTailwindBinary(path:))
      }

    Error(_) -> {
      use name <- result.try(detect_platform())
      let path =
        project.bin
        |> filepath.join(name)
        |> filepath.join(system.executable_name(name))

      // Detect if we've already downloaded Tailwind because of an earlier command.
      // If not, we automatically download it now.
      let timeout =
        tom.get_int(project.options, ["bin", "timeout"])
        |> result.map(fn(seconds) { seconds * 1000 })
        |> result.unwrap(60_000)

      use path <- result.try(case simplifile.is_file(path) {
        Ok(True) -> {
          case verify_version(path) {
            Ok(_) -> Ok(path)
            Error(_) -> download(project, False, quiet, timeout)
          }
        }
        _ -> download(project, False, quiet, timeout)
      })

      Ok(path)
    }
  }
}

/// Detect the user's operating system and CPU architecture, and then resolve the
/// name of the Tailwind binary we need to download.
///
fn detect_platform() -> Result(String, Error) {
  let os = system.detect_os()
  let arch = system.detect_arch()

  resolve(os, arch)
  |> result.replace_error(error.UnsupportedPlatform(os:, arch:))
}

/// Verifies the integrity of the downloaded archive by comparing its SHA-256
/// hash against the hashes provided by Tailwind:
///
///   https://github.com/tailwindlabs/tailwindcss/releases/download/v4.3.3/sha256sums.txt
///
fn verify_integrity(archive: BitArray, name: String) -> Result(Nil, Error) {
  let assert Ok(expected) = list.key_find(hashes, name)
  let hash = crypto.hash(crypto.Sha256, archive)
  let actual = bit_array.base16_encode(hash) |> string.lowercase

  case actual == expected {
    True -> Ok(Nil)
    False -> Error(error.CouldNotVerifyTailwindHash(expected:, actual:))
  }
}

/// Resolve the full name of the Tailwind archive we need to download based on the
/// system's OS and CPU architecture. This will get us something like
/// `"tailwindcss-macos-x64"` or `"tailwindcss-linux-x64-musl"`.
///
fn resolve(os: String, arch: String) -> Result(String, Nil) {
  let is_alpine = system.is_alpine()

  case os, arch {
    "darwin", "aarch64" | "darwin", "arm64" -> Ok("tailwindcss-macos-arm64")
    "darwin", "x64" | "darwin", "x86_64" -> Ok("tailwindcss-macos-x64")
    "linux", "aarch64" | "linux", "arm64" if is_alpine ->
      Ok("tailwindcss-linux-arm64-musl")
    "linux", "aarch64" | "linux", "arm64" -> Ok("tailwindcss-linux-arm64")
    "linux", "x64" | "linux", "x86_64" if is_alpine ->
      Ok("tailwindcss-linux-x64-musl")
    "linux", "x64" | "linux", "x86_64" -> Ok("tailwindcss-linux-x64")
    "win32", "x64"
    | "windows", "x64"
    | "win32", "x86_64"
    | "windows", "x86_64"
    -> Ok("tailwindcss-windows-x64")
    _, _ -> Error(Nil)
  }
}

/// Verifies that the version of the Tailwind binary we have matches the version
/// we support. This is particularly useful in cases where the user provides a
/// path to their own binary.
///
fn verify_version(path: String) -> Result(Nil, Error) {
  use output <- result.try(
    system.run(path, ["--help"], [#("NO_COLOR", "1")])
    |> result.replace_error(error.CouldNotLocateTailwindBinary(path:)),
  )

  use #(actual, _) <- result.try(
    string.split_once(output, "\n")
    |> result.replace_error(error.UnsupportedTailwindVersion(
      path:,
      expected: version,
      actual: "unknown",
    )),
  )

  // Tailwind pulls some bullshit here. They don't provide an actual `-v` or
  // `--version` flag, so we have to parse out the first line of the help output
  // where they coincidentally print the version.
  case ansi.strip(actual) {
    "≈ tailwindcss v" <> v if v == version -> Ok(Nil)

    "≈ tailwindcss v" <> v ->
      Error(error.UnsupportedTailwindVersion(
        path:,
        expected: version,
        actual: v,
      ))

    _ ->
      Error(error.UnsupportedTailwindVersion(
        path:,
        expected: version,
        actual: "unknown",
      ))
  }
}

// CONSTANTS -------------------------------------------------------------------

const version = "4.3.3"

const hashes = [
  #(
    "tailwindcss-linux-arm64",
    "55fd0b241214eff3de1e8ee4f22796662f2d2e7a49bcfca7477cfd0bac398195",
  ),
  #(
    "tailwindcss-linux-arm64-musl",
    "71ea4be79c9de9827545682df3e040053fb535d37c71ed2cfdedf9385a0868e0",
  ),
  #(
    "tailwindcss-linux-x64",
    "dc61b3ac6b8c9ca874c0cc4c57b2409791a64c5540404ca5f5367360babc313a",
  ),
  #(
    "tailwindcss-linux-x64-musl",
    "a04d34ceacc8f52cbe8920ad846cdeb61d3d0021dba32db0d1f77c9d9fad7a6c",
  ),
  #(
    "tailwindcss-macos-arm64",
    "cdf646702987a743464dff4d9c60fd4480d1c1e73dd819a9a67f1078815dce9d",
  ),
  #(
    "tailwindcss-macos-x64",
    "7922e0953f2110c05976e3bf58f14e643d90427575e766b7d433f5f80cbee7e1",
  ),
  #(
    "tailwindcss-windows-x64",
    "e0e260ce048014e9268f6237ff18f8ccf02cef521cbd0ae04e82c2cdf7aa3955",
  ),
]
