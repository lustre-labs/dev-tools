// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/bool
import gleam/dynamic.{type Dynamic}
import gleam/io
import gleam/result
import gleam/set
import gleam/string
import lustre_dev_tools/project
import lustre_dev_tools/cli.{type Cli}
import simplifile.{type FilePermissions, Execute, FilePermissions, Read, Write}

const tailwind_version = "v3.4.1"

// COMMANDS --------------------------------------------------------------------

pub fn setup(os: String, cpu: String) -> Cli(any, Nil, Error) {
  use _ <- cli.do(download(os, cpu, tailwind_version))
  use _ <- cli.do(write_tailwind_config())

  cli.return(Nil)
}

fn download(os: String, cpu: String, version: String) -> Cli(any, Nil, Error) {
  use <- cli.log("Downloading Tailwind")

  let root = project.root()
  let outdir = filepath.join(root, "build/.lustre/bin")
  let outfile = filepath.join(outdir, "tailwind")

  case check_tailwind_exists(outfile) {
    True -> cli.success("Tailwind already installed!", fn() { cli.return(Nil) })
    False -> {
      use <- cli.log("Detecting platform")
      use url <- cli.do_result(get_download_url(os, cpu, version))

      use <- cli.log("Downloading from " <> url)
      use bin <- cli.try(get_tailwind(url), NetworkError)

      use _ <- cli.try(write_tailwind(bin, outdir, outfile), fn(reason) {
        CannotWriteTailwind(reason, outfile)
      })
      use _ <- cli.try(set_filepermissions(outfile), fn(reason) {
        CannotSetPermissions(reason, outfile)
      })

      use <- cli.success("Tailwind installed!")

      cli.return(Nil)
    }
  }
}

fn write_tailwind_config() -> Cli(any, Nil, Error) {
  let config_filename = "tailwind.config.js"
  let config_outfile = filepath.join(project.root(), config_filename)
  let config_already_exists =
    simplifile.verify_is_file(config_outfile)
    |> result.unwrap(False)

  // If there already is a configuration file, we make sure not to override it.
  use <- bool.guard(when: config_already_exists, return: cli.return(Nil))
  use <- cli.log("Writing `" <> config_filename <> "`")
  use config <- cli.template("tailwind.config.js", InternalError)
  use _ <- cli.try(
    simplifile.write(to: config_outfile, contents: config),
    CannotWriteConfig(_, config_outfile),
  )
  use <- cli.success("Written `" <> config_outfile <> "`")

  cli.return(Nil)
}

// STEPS -----------------------------------------------------------------------

fn check_tailwind_exists(path) {
  case simplifile.verify_is_file(path) {
    Ok(True) -> True
    Ok(False) | Error(_) -> False
  }
}

fn get_download_url(os, cpu, version) {
  let base =
    "https://github.com/tailwindlabs/tailwindcss/releases/download/"
    <> version
    <> "/tailwindcss-"

  let path = case os, cpu {
    "linux", "armv7" -> Ok("linux-armv7")
    "linux", "arm64" -> Ok("linux-arm64")
    "linux", "x64" | "linux", "x86_64" -> Ok("linux-x64")

    "win32", "arm64" -> Ok("windows-arm64.exe")
    "win32", "x64" | "win32", "x86_64" -> Ok("windows-x64.exe")

    "darwin", "arm64" | "darwin", "aarch64" -> Ok("macos-arm64")
    "darwin", "x64" | "darwin", "x86_64" -> Ok("macos-x64")

    _, _ -> Error(UnknownPlatform(os, cpu))
  }

  result.map(path, string.append(base, _))
}

fn write_tailwind(bin, outdir, outfile) {
  let _ = simplifile.create_directory_all(outdir)

  simplifile.write_bits(outfile, bin)
}

fn set_filepermissions(file) {
  let permissions =
    FilePermissions(
      user: set.from_list([Read, Write, Execute]),
      group: set.from_list([Read, Execute]),
      other: set.from_list([Read, Execute]),
    )

  simplifile.set_permissions(file, permissions)
}

// ERROR HANDLING --------------------------------------------------------------

pub type Error {
  NetworkError(Dynamic)
  CannotWriteTailwind(reason: simplifile.FileError, path: String)
  CannotSetPermissions(reason: simplifile.FileError, path: String)
  CannotWriteConfig(reason: simplifile.FileError, path: String)
  UnknownPlatform(os: String, cpu: String)
  BundleError(reason: String)
  InternalError(message: String)
}

pub fn explain(error: Error) -> Nil {
  case error {
    // TODO: Is there a better way to deal with this dynamic error?
    NetworkError(_dynamic) ->
      io.println(
        "
There was a network error!",
      )

    UnknownPlatform(os, cpu) -> io.println("
I couldn't figure out the correct Tailwind version for your
os (" <> os <> ") and cpu (" <> cpu <> ").")

    CannotSetPermissions(reason, _) -> io.println("
I ran into an error (" <> string.inspect(reason) <> ") when trying
to set permissions for the Tailwind executable.
")

    CannotWriteConfig(reason, _) -> io.println("
I ran into an error (" <> string.inspect(reason) <> ") when trying
to write the `tailwind.config.js` file to the project's root.
")

    CannotWriteTailwind(reason, path) -> io.println("
I ran into an error (" <> string.inspect(reason) <> ") when trying
to write the Tailwind binary to
  `" <> path <> "`.
")

    BundleError(reason) -> io.println("
I ran into an error while trying to create a bundle with Tailwind:
" <> reason)

    InternalError(message) -> io.println("
I ran into an error I wasn't expecting. Please open an issue on GitHub at
https://github.com/lustre-labs/cli with the following message:
" <> message)
  }
}

// EXTERNALS -------------------------------------------------------------------

@external(erlang, "lustre_dev_tools_ffi", "get_esbuild")
fn get_tailwind(url: String) -> Result(BitArray, Dynamic)
