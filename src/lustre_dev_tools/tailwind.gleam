// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/bit_array
import gleam/bool
import gleam/crypto
import gleam/dynamic.{type Dynamic}
import gleam/result
import gleam/set
import gleam/string
import gleam_community/ansi
import glint
import lustre_dev_tools/cli.{type Cli}
import lustre_dev_tools/cli/flag
import lustre_dev_tools/cmd
import lustre_dev_tools/error.{
  type Error, BundleError, CannotSetPermissions, CannotWriteFile, NetworkError,
  UnknownPlatform,
}
import lustre_dev_tools/project
import lustre_dev_tools/utils
import simplifile.{Execute, FilePermissions, Read, Write}

// COMMANDS --------------------------------------------------------------------

pub fn get_entry_file_and_then(root, next_fun) {
  use project_name <- cli.do(cli.get_name())
  let default_entryfile = filepath.join(root, "src/" <> project_name <> ".css")
  use entryfile <- cli.do(
    cli.get_string(
      "tailwind-entry",
      default_entryfile,
      ["build"],
      glint.get_flag(_, flag.tailwind_entry()),
    ))

  next_fun(entryfile)
}

pub fn maybe_bundle(
  outfile: String,
  bundle_tailwind_flag: Bool,
  minify: Bool,
) -> Cli(Nil) {
  use <- bool.guard(when: !bundle_tailwind_flag, return: cli.return(Nil))

  use has_legacy_config_file <- cli.do(detect_legacy_config())

  let root = project.root()
  use entryfile <- get_entry_file_and_then(root)

  let has_css_import =
    simplifile.read(entryfile)
    |> result.map(has_tailwind_imported)
    |> result.unwrap(False)

  // This will if a line contains @import*tailwindcss*;
  // where the user is not importing all tailwind default imports.
  case has_legacy_config_file, has_css_import {
    True, False -> {
      use _ <- cli.do(init_tailwind_css(entryfile))
      use _ <- cli.do(bundle(outfile, root, entryfile, minify))
      cli.return(Nil)
    }
    _any, True -> {
      use _ <- cli.do(bundle(outfile, root, entryfile, minify))
      cli.return(Nil)
    }
    False, False -> {
      cli.return(Nil)
    }
  }
}

pub fn init_tailwind_css(entryfile: String) -> Cli(Nil) {
  case simplifile.read(entryfile) {
    Ok(entry_css) -> {
      let entry_css = "@import \"tailwindcss\";\n" <> entry_css
      use <- cli.log("Adding Tailwind integration to " <> entryfile)
      use _ <- cli.try(
        simplifile.write(entryfile, entry_css)
        |> result.map_error(CannotWriteFile(_, entryfile)),
      )
      use <- cli.success(entryfile <> " updated!")

      cli.return(Nil)
    }
    Error(_) -> {
      use <- cli.log("Generating Tailwind config")
      use _ <- cli.try(
        simplifile.write(entryfile, "@import \"tailwindcss\";\n")
        |> result.map_error(CannotWriteFile(_, entryfile)),
      )
      use <- cli.success("Tailwind succeessfully configured!")
      use _ <- cli.do(display_next_steps())

      cli.return(Nil)
    }
  }
}

fn bundle(
  outfile: String,
  root: String,
  entryfile: String,
  minify: Bool,
) -> Cli(Nil) {
  use _ <- cli.do(download())

  use <- cli.log("Bundling with Tailwind")

  let flags = ["--input=" <> entryfile, "--output=" <> outfile]
  let options = case minify {
    True -> ["--minify", ..flags]
    False -> flags
  }
  use _ <- cli.try(exec_tailwind(root, options))
  use <- cli.success("Bundle produced at `" <> outfile <> "`")

  cli.return(Nil)
}

fn exec_tailwind(root: String, options: List(String)) -> Result(String, Error) {
  cmd.exec("./build/.lustre/bin/tailwind", in: root, env: [], with: options)
  |> result.map_error(fn(pair) { BundleError(pair.1) })
}

fn has_tailwind_imported(file_str: String) {
  file_str |> string.to_graphemes |> has_tailwind_imported_rec("")
}

fn has_tailwind_imported_rec(graphemes: List(String), current_line: String) {
  let check_if_imported = fn(first, second, rest) {
    case first == "@import" && string.contains(second, "tailwindcss") {
      True -> True
      False -> has_tailwind_imported_rec(rest, "")
    }
  }

  case graphemes {
    [grapheme, ..rest] if grapheme == ";" -> {
      case string.split(current_line, " ") {
        [first, second, ..] -> check_if_imported(first, second, rest)
        _else -> has_tailwind_imported_rec(rest, "")
      }
    }
    [grapheme, ..rest] ->
      has_tailwind_imported_rec(rest, string.append(current_line, grapheme))
    [] -> False
  }
}

fn download() -> Cli(Nil) {
  let os = get_os()
  let cpu = get_cpu()

  use <- cli.log("Downloading Tailwind")

  let root = project.root()
  let outdir = filepath.join(root, "build/.lustre/bin")
  let outfile = filepath.join(outdir, "tailwind")

  case check_tailwind_exists(outfile, os, cpu) {
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

fn detect_legacy_config() -> Cli(Bool) {
  let root = project.root()
  use config <- cli.try(project.config())
  let old_config_path = filepath.join(root, "tailwind.config.js")

  case simplifile.is_file(old_config_path) {
    Ok(True) -> {
      use <- cli.notify(ansi.bold("\nLegacy Tailwind config detected:"))
      use <- cli.notify(
        "
Lustre Dev Tools now only supports Tailwind CSS v4.0 and above. If you are not
ready to migrate your config to the new format, you can continue using JavaScript
config by including the `@config` directive in your `src/{app_name}.css` file.

See: https://tailwindcss.com/docs/upgrade-guide#using-a-javascript-config-file

You can supress this message by removing `tailwind.config.js` from your project.
"
        |> string.trim
        |> string.replace("{app_name}", config.name),
      )

      cli.return(True)
    }
    Ok(False) | Error(_) -> cli.return(False)
  }
}

fn display_next_steps() -> Cli(Nil) {
  use config <- cli.try(project.config())
  use <- cli.notify(ansi.bold("\nNext Steps:"))
  use <- cli.notify(
    "
1. Be sure to update your root `index.html` file to include
   <link rel=\"stylesheet\" type=\"text/css\" href=\"./priv/static/{app_name}.css\" />"
    |> string.trim
    |> string.replace("{app_name}", config.name),
  )

  cli.return(Nil)
}

// STEPS -----------------------------------------------------------------------

fn check_tailwind_exists(path, os, cpu) {
  case simplifile.is_file(path) {
    Ok(True) ->
      result.unwrap(
        {
          use #(_, hash) <- result.try(get_download_url_and_hash(os, cpu))
          use bin <- result.try(
            simplifile.read_bits(path)
            |> result.replace_error(error.InvalidTailwindBinary),
          )
          use _ <- result.try(check_tailwind_integrity(bin, hash))

          Ok(True)
        },
        False,
      )

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

@external(erlang, "lustre_dev_tools_ffi", "get_os")
fn get_os() -> String

@external(erlang, "lustre_dev_tools_ffi", "get_cpu")
fn get_cpu() -> String
