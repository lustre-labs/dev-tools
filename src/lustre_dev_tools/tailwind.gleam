// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/bit_array
import gleam/bool
import gleam/crypto
import gleam/dynamic.{type Dynamic}
import gleam/result
import gleam/set
import gleam_community/ansi
import lustre_dev_tools/cli.{type Cli}
import lustre_dev_tools/error.{
  type Error, CannotSetPermissions, CannotWriteFile, NetworkError,
  UnknownPlatform,
}
import lustre_dev_tools/project
import lustre_dev_tools/utils
import simplifile.{Execute, FilePermissions, Read, Write}

// COMMANDS --------------------------------------------------------------------

pub fn setup(os: String, cpu: String) -> Cli(Nil) {
  use _ <- cli.do(download(os, cpu))
  use _ <- cli.do(write_tailwind_config())
  use _ <- cli.do(display_next_steps())

  cli.return(Nil)
}

fn download(os: String, cpu: String) -> Cli(Nil) {
  use <- cli.log("Downloading Tailwind")

  let root = project.root()
  let outdir = filepath.join(root, "build/.lustre/bin")
  let outfile = filepath.join(outdir, "tailwind")

  case check_tailwind_exists(outfile) {
    True -> cli.success("Tailwind already installed!", fn() { cli.return(Nil) })
    False -> {
      use <- cli.log("Detecting platform")
      use #(url, hash) <- cli.try(get_download_url_and_hash(os, cpu))

      // We want to fit the url in the space remaining after the
      // "⠸ Downloading from ": that takes 19 chars of the terminal width.
      let max_url_size = utils.term_width() - 19
      let shortened_url = utils.shorten_url(url, to: max_url_size)
      use <- cli.log("Downloading from " <> shortened_url)
      use bin <- cli.try(get_tailwind(url))

      use <- cli.log("Checking the downloaded Tailwind binary")
      use _ <- cli.try(check_tailwind_integrity(bin, hash))

      use _ <- cli.try(write_tailwind(bin, outdir, outfile))
      use _ <- cli.try(set_file_permissions(outfile))
      use <- cli.success("Tailwind installed!")

      cli.return(Nil)
    }
  }
}

fn display_next_steps() -> Cli(Nil) {
  use <- cli.notify(ansi.bold("\nNext Steps:\n"))
  use <- cli.notify(
    "1. Be sure to update your root `index.html` file to include \n   `<link rel='stylesheet' type='text/css' href='./priv/static/your_app.css' />`",
  )
  cli.return(Nil)
}

fn write_tailwind_config() -> Cli(Nil) {
  let config_filename = "tailwind.config.js"
  let config_outfile = filepath.join(project.root(), config_filename)
  let config_already_exists =
    simplifile.is_file(config_outfile)
    |> result.unwrap(False)

  // If there already is a configuration file, we make sure not to override it.
  use <- bool.guard(when: config_already_exists, return: cli.return(Nil))
  use <- cli.log("Writing `" <> config_filename <> "`")
  use config <- cli.template("tailwind.config.js")
  use _ <- cli.try(write_config(config_outfile, config))
  use <- cli.success("Written `" <> config_outfile <> "`")

  cli.return(Nil)
}

// STEPS -----------------------------------------------------------------------

fn check_tailwind_exists(path) {
  case simplifile.is_file(path) {
    Ok(True) -> True
    Ok(False) | Error(_) -> False
  }
}

/// Returns the appropriate url to download the tailwind binary and the expected
/// base16-encoded sha256 hash to check the downloaded binary's integrity.
///
fn get_download_url_and_hash(os, cpu) {
  let base =
    "https://github.com/tailwindlabs/tailwindcss/releases/download/v3.4.1/tailwindcss-"

  case os, cpu {
    "linux", "armv7" ->
      Ok(#(
        base <> "linux-armv7",
        "38E004B144004495CD148621ADB852C21D5D350E66308C8FF9E2FD90A15726F5",
      ))
    "linux", "arm64" | "linux", "aarch64" ->
      Ok(#(
        base <> "linux-arm64",
        "1178C3E8B44B9EB43F40E786EE25664C93D83F6D05B062C0D9CAF410D64D5587",
      ))
    "linux", "x64" | "linux", "x86_64" ->
      Ok(#(
        base <> "linux-x64",
        "A6814CC8FB6E573DD637352093F3B8E927C5C8628B1FF87826652935AF1430B1",
      ))

    "win32", "arm64" ->
      Ok(#(
        base <> "windows-arm64.exe",
        "AC06BC274FAED7A0C9C0C7E9058D87A428B574BCF8FCE85330F576DE4568BB81",
      ))
    "win32", "x64" | "win32", "x86_64" ->
      Ok(#(
        base <> "windows-x64.exe",
        "EF2FE367DAA8204CB186796C1833FAEE81A5B20E4C80E533F7A3B3DCC7DB6C54",
      ))

    "darwin", "arm64" | "darwin", "aarch64" ->
      Ok(#(
        base <> "macos-arm64",
        "40738E59ECEF06F955243154E7D1C6EAF11370037CBEEE4A32C3138387E2DA5D",
      ))
    "darwin", "x64" | "darwin", "x86_64" ->
      Ok(#(
        base <> "macos-x64",
        "594D01B032125199DB105C661FE23DE4C069006921B96F7FEE98EE4FBC15F800",
      ))

    _, _ -> Error(UnknownPlatform("tailwind", os, cpu))
  }
}

fn get_tailwind(url: String) -> Result(BitArray, Error) {
  do_get_tailwind(url)
  |> result.map_error(NetworkError)
}

fn check_tailwind_integrity(bin: BitArray, expected_hash: String) {
  let hash = crypto.hash(crypto.Sha256, bin)
  let hash_string = bit_array.base16_encode(hash)
  case hash_string == expected_hash {
    True -> Ok(Nil)
    False -> Error(error.InvalidTailwindBinary)
  }
}

fn write_tailwind(
  bin: BitArray,
  outdir: String,
  outfile: String,
) -> Result(Nil, Error) {
  let _ = simplifile.create_directory_all(outdir)

  simplifile.write_bits(outfile, bin)
  |> result.map_error(CannotWriteFile(_, filepath.join(outdir, outfile)))
}

fn set_file_permissions(file: String) -> Result(Nil, Error) {
  let permissions =
    FilePermissions(
      user: set.from_list([Read, Write, Execute]),
      group: set.from_list([Read, Execute]),
      other: set.from_list([Read, Execute]),
    )

  simplifile.set_permissions(file, permissions)
  |> result.map_error(CannotSetPermissions(_, file))
}

fn write_config(path: String, content: String) -> Result(Nil, Error) {
  simplifile.write(to: path, contents: content)
  |> result.map_error(CannotWriteFile(_, path))
}

// EXTERNALS -------------------------------------------------------------------

@external(erlang, "lustre_dev_tools_ffi", "get_esbuild")
fn do_get_tailwind(url: String) -> Result(BitArray, Dynamic)
