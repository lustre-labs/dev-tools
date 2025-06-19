// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/bit_array
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
    "1. Create an app.css file with '@import \"tailwindcss\"' to enable Tailwind CSS.\n2. Be sure to update your root `index.html` file to include \n   `<link rel='stylesheet' type='text/css' href='./priv/static/your_app.css' />`",
  )
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
    "https://github.com/tailwindlabs/tailwindcss/releases/download/v4.1.10/tailwindcss-"

  case os, cpu {
    "linux", "arm64" | "linux", "aarch64" ->
      Ok(#(
        base <> "linux-arm64",
        "67EB620BB404C2046D3C127DBF2D7F9921595065475E7D2D528E39C1BB33C9B6",
      ))
    "linux", "x64" | "linux", "x86_64" ->
      Ok(#(
        base <> "linux-x64",
        "0A85A3E533F2E7983BDB91C08EA44F0EAB3BECC275E60B3BAADDF18F71D390BF",
      ))

    "win32", "x64" | "win32", "x86_64" ->
      Ok(#(
        base <> "windows-x64.exe",
        "5539346428771D8974AC63B68D1F477866BECECF615B3A14F2F197A36BDAAC33",
      ))

    "darwin", "arm64" | "darwin", "aarch64" ->
      Ok(#(
        base <> "macos-arm64",
        "F34A85A75B1F2DE2C7E4A9FBC4FB976E64A2780980E843DF87D9C13F555F4A4C",
      ))
    "darwin", "x64" | "darwin", "x86_64" ->
      Ok(#(
        base <> "macos-x64",
        "47A130C5F639384456E0AC8A0D60B95D74906187314A4DBC37E7C1DDBEB713AE",
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

// EXTERNALS -------------------------------------------------------------------

@external(erlang, "lustre_dev_tools_ffi", "get_esbuild")
fn do_get_tailwind(url: String) -> Result(BitArray, Dynamic)
