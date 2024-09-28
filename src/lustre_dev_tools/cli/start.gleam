// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/io
import gleam/result
import gleam/string
import glint.{type Command}
import lustre_dev_tools/cli.{type Cli, do, try}
import lustre_dev_tools/cli/build
import lustre_dev_tools/cli/flag
import lustre_dev_tools/cmd
import lustre_dev_tools/error.{type Error, CannotWriteFile}
import lustre_dev_tools/project
import lustre_dev_tools/server
import simplifile

// COMMANDS --------------------------------------------------------------------

pub fn run() -> Command(Nil) {
  let description =
    "
Start a development server for your Lustre project. This command will compile your
application and serve it on a local server.
    "
  use <- glint.command_help(description)
  use <- glint.unnamed_args(glint.EqArgs(0))
  use port <- glint.flag(flag.port())
  use _proxy_from <- glint.flag(flag.proxy_from())
  use _proxy_to <- glint.flag(flag.proxy_to())
  use detect_tailwind <- glint.flag(flag.detect_tailwind())
  use _tailwind_entry <- glint.flag(flag.tailwind_entry())
  use _, _, flags <- glint.command()
  let script = {
    use port <- do(cli.get_int("port", 1234, ["start"], port))
    use detect_tailwind <- do(cli.get_bool(
      "detect_tailwind",
      True,
      ["build"],
      detect_tailwind,
    ))

    use _ <- do(check_otp_version())
    use _ <- do(build.do_app(False, detect_tailwind, False))
    use _ <- do(prepare_html())
    use _ <- do(server.start(port))

    cli.return(Nil)
  }

  case cli.run(script, flags) {
    Ok(_) -> Nil
    Error(error) -> error.explain(error) |> io.print_error
  }
}

// STEPS -----------------------------------------------------------------------

fn check_otp_version() -> Cli(Nil) {
  use <- cli.log("Checking OTP version")
  let version = project.otp_version()
  case version <= 25 {
    False -> cli.return(Nil)
    True -> cli.throw(error.OtpTooOld(version))
  }
}

fn prepare_html() -> Cli(Nil) {
  let assert Ok(cwd) = cmd.cwd()
  let assert Ok(root) = filepath.expand(filepath.join(cwd, project.root()))
  let index = filepath.join(root, "index.html")

  case simplifile.is_file(index) {
    Ok(True) -> cli.return(Nil)
    Ok(False) | Error(_) -> {
      use html <- cli.template("index.html")
      use app_name <- do(cli.get_name())
      let html = string.replace(html, "{app_name}", app_name)
      use _ <- try(write_html(index, html))

      cli.return(Nil)
    }
  }
}

fn write_html(path: String, source: String) -> Result(Nil, Error) {
  simplifile.write(path, source)
  |> result.map_error(CannotWriteFile(_, path))
}
