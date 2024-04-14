// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/result
import gleam/string
import glint.{type Command, CommandInput}
import glint/flag
import lustre_dev_tools/cli.{type Cli, do, try}
import lustre_dev_tools/cli/build
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
application and serve it on a local server. If your application's `main` function
returns a compatible `App`, this will generate the necessary code to start it.
Otherwise, your `main` function will be used as the entry point.

This development server does *not* currently watch your files for changes.
Watchexec is a popular tool you can use to restart the server when files change.
    "

  glint.command(fn(input) {
    let CommandInput(flags: flags, ..) = input
    let script = {
      use port <- do(cli.get_int("port", 1234, ["start"]))

      use _ <- do(build.do_app(False))
      use _ <- do(prepare_html())
      use _ <- try(server.start(port))

      cli.return(Nil)
    }

    case cli.run(script, flags) {
      Ok(_) -> Nil
      Error(error) -> error.explain(error)
    }
  })
  |> glint.description(description)
  |> glint.unnamed_args(glint.EqArgs(0))
  |> glint.flag("port", {
    let description =
      "Specify server port. If the port is taken the dev server will not start."

    flag.int()
    |> flag.description(description)
  })
}

// STEPS -----------------------------------------------------------------------

fn prepare_html() -> Cli(Nil) {
  let assert Ok(cwd) = cmd.cwd()
  let assert Ok(root) = filepath.expand(filepath.join(cwd, project.root()))
  let index = filepath.join(root, "index.html")

  case simplifile.verify_is_file(index) {
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
