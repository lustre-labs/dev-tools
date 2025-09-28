// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/bit_array
import gleam/bool
import gleam/crypto
import gleam/http.{Get, Https}
import gleam/http/request.{Request}
import gleam/httpc
import gleam/list
import gleam/option.{None}
import gleam/regexp
import gleam/result
import gleam/string
import lustre_dev_tools/cli
import lustre_dev_tools/error.{type Error}
import lustre_dev_tools/project.{type Project}
import lustre_dev_tools/system
import simplifile
import tom

// TYPES -----------------------------------------------------------------------

///
///
pub type Detection {
  HasViableEntry
  HasTailwindEntry
  HasLegacyConfig
  Nothing
}

//

///
///
pub fn download(project: Project, quiet quiet: Bool) -> Result(Nil, Error) {
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
        <> name,
      query: None,
    )

  cli.log("Downloading TailwindCSS v" <> version, quiet)

  use res <- result.try(
    httpc.configure()
    // The Tailwind binary is uncompressed and ~100mb so we'll bump the timeout
    // up to a minute to be safe.
    |> httpc.timeout(60_000)
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
    simplifile.create_directory(path)
    |> result.map_error(error.CouldNotWriteFile(path:, reason: _)),
  )

  let path = filepath.join(path, "tailwindcss")

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

  cli.success("TailwindCSS v" <> version <> " is ready to go!", quiet)

  // 7.
  use detection <- result.try(detect(project, project.name))
  let path = filepath.join(project.src, project.name <> ".css")

  case detection {
    HasTailwindEntry -> Ok(Nil)

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

      Ok(Nil)
    }

    HasViableEntry -> {
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

      Ok(Nil)
    }

    Nothing -> {
      let css = ["@import \"tailwindcss\";", ""]

      use _ <- result.try(
        simplifile.write(path, string.join(css, "\n"))
        |> result.map_error(error.CouldNotWriteFile(path:, reason: _)),
      )

      cli.success("Tailwind config generated", quiet)

      Ok(Nil)
    }
  }
}

///
///
pub fn detect(project: Project, entry: String) -> Result(Detection, Error) {
  // 1. Check if the user has a CSS file that is - or could be - the entry for
  //    Tailwind.
  let tailwind_config = filepath.join(project.src, entry <> ".css")
  let has_tailwind_config =
    tailwind_config
    |> simplifile.is_file
    |> result.unwrap(False)

  // 2. Detect if they have a legacy Tailwind v3 config file, which is
  //    deprecated but still supported in v4.
  let legacy_config = filepath.join(project.root, "tailwind.config.js")
  let has_legacy_config =
    legacy_config
    |> simplifile.is_file
    |> result.unwrap(False)

  case has_tailwind_config {
    True -> {
      use css <- result.try(
        simplifile.read(tailwind_config)
        |> result.map_error(error.CouldNotReadFile(tailwind_config, _)),
      )

      let assert Ok(re) = regexp.from_string("^@import\\s+['\"]tailwindcss")
      case regexp.check(re, css) {
        True -> Ok(HasTailwindEntry)
        False -> Ok(HasViableEntry)
      }
    }
    False if has_legacy_config -> Ok(HasLegacyConfig)
    False -> Ok(Nothing)
  }
}

pub fn build(
  project project: Project,
  input in: String,
  outdir out: String,
  minify minify: Bool,
  quiet quiet: Bool,
) -> Result(Nil, Error) {
  use path <- result.try(guard(project, quiet:))
  let name = filepath.base_name(in)
  let output = filepath.join(project.root, out) |> filepath.join(name)
  let minify = bool.guard(minify, "--minify", fn() { "" })
  let flags = [
    "-i " <> filepath.join(project.src, in <> ".css"),
    "-o " <> output <> ".css",
    minify,
  ]

  use _ <- result.try(
    system.run(path <> " " <> string.join(flags, " "))
    |> result.map_error(error.FailedToBuildProject),
  )

  Ok(Nil)
}

// UTILS -----------------------------------------------------------------------

///
///
fn guard(project: Project, quiet quiet: Bool) -> Result(String, Error) {
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
      let path = filepath.join(project.bin, name <> "/tailwindcss")

      // Detect if we've already downloaded Tailwind because of an earlier command.
      // If not, we automatically download it now.
      use _ <- result.try(case simplifile.is_file(path) {
        Ok(True) ->
          case verify_version(path) {
            Ok(_) -> Ok(Nil)
            Error(_) -> download(project, quiet:)
          }

        Ok(False) | Error(_) -> download(project, quiet:)
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
///   https://github.com/tailwindlabs/tailwindcss/releases/download/v4.1.13/sha256sums.txt
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
    "darwin", "aarch64" -> Ok("tailwindcss-macos-arm64")
    "darwin", "x64" -> Ok("tailwindcss-macos-x64")
    "linux", "aarch64" if is_alpine -> Ok("tailwindcss-linux-arm64-musl")
    "linux", "aarch64" -> Ok("tailwindcss-linux-arm64")
    "linux", "arm64" if is_alpine -> Ok("tailwindcss-linux-arm64-musl")
    "linux", "arm64" -> Ok("tailwindcss-linux-arm64")
    "linux", "x64" if is_alpine -> Ok("tailwindcss-linux-x64-musl")
    "linux", "x64" -> Ok("tailwindcss-linux-x64")
    "windows", "x64" -> Ok("tailwindcss-windows-x64.exe")
    _, _ -> Error(Nil)
  }
}

/// Verifies that the version of the Tailwind binary we have matches the version
/// we support. This is particularly useful in cases where the user provides a
/// path to their own binary.
///
fn verify_version(path: String) -> Result(Nil, Error) {
  use output <- result.try(
    system.run(path <> " --help")
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
  case actual {
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

const version = "4.1.13"

const hashes = [
  #(
    "tailwindcss-linux-arm64",
    "c90529475a398adbf3315898721c0f9fe85f434a2b3ea3eafada68867641819a",
  ),
  #(
    "tailwindcss-linux-arm64-musl",
    "09624e1cb6295849020fb78344eb5dfa8196f57dfa6f81a0cb8442151d2f860d",
  ),
  #(
    "tailwindcss-linux-x64",
    "b9ed9f8f640d3323711f9f68608aa266dff3adbc42e867c38ea2d009b973be11",
  ),
  #(
    "tailwindcss-linux-x64-musl",
    "5785fbf6bc1e489e0d3c9743aa6cf0fe20b4633c1de6e8a4d6cdc0bf86716d71",
  ),
  #(
    "tailwindcss-macos-arm64",
    "c47681e9948db20026a913a4aca4ee0269b4c0d4ef3f71343cb891dfdc1e97c9",
  ),
  #(
    "tailwindcss-macos-x64",
    "c3b230bdbfaa46c94cad8db44da1f82773f10bac54f56fa196c8977d819c09e4",
  ),
  #(
    "tailwindcss-windows-x64.exe",
    "ad16a528e13111e5df4e771b4b4981bd4b73e69140fa021f4102f46f02eeb86d",
  ),
]
