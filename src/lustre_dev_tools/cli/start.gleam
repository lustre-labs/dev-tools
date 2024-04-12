// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/string
import glint.{type Command, CommandInput}
import glint/flag
import lustre_dev_tools/cli.{type Cli}
import lustre_dev_tools/cli/build
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
    let assert Ok(port) = flag.get_int(flags, "port")
    let assert Ok(use_example_styles) =
      flag.get_bool(flags, "use-example-styles")

    let script = {
      use _ <- cli.do(build.do_app(False))
      use _ <- cli.do(prepare_html(use_example_styles))
      use _ <- cli.do(cli.from_result(server.start(port)))

      cli.return(Nil)
    }

    case cli.run(script, Nil) {
      Ok(_) -> Nil
      Error(error) -> error.explain(error)
    }
  })
  |> glint.description(description)
  |> glint.unnamed_args(glint.EqArgs(0))
  |> glint.flag("port", {
    let description =
      "Specify server port. If the port is taken the dev server will not start."
    let default = 1234

    flag.int()
    |> flag.default(default)
    |> flag.description(description)
  })
  |> glint.flag("use-example-styles", {
    let description =
      "Inject the lustre/ui stylesheet. This is primarily intended to be used when running any of Lustre's examples and is ignored if the `--html` flag is set."
    let default = False

    flag.bool()
    |> flag.default(default)
    |> flag.description(description)
  })
  |> glint.flag("html", {
    let description =
      "Supply a custom HTML file to use as the entry point. To inject the Lustre bundle, make sure it includes the following empty script: <script type=\"application/lustre\"></script>"
      |> string.trim_right

    flag.string()
    |> flag.description(description)
  })
}

// STEPS -----------------------------------------------------------------------

fn prepare_html(include_example_styles: Bool) -> Cli(any, Nil, Error) {
  let assert Ok(cwd) = cli.cwd()
  let assert Ok(root) = filepath.expand(filepath.join(cwd, project.root()))
  let index = filepath.join(root, "index.html")

  case simplifile.verify_is_file(index) {
    Ok(True) -> cli.return(Nil)
    Ok(False) | Error(_) -> {
      use html <- cli.template(case include_example_styles {
        True -> "index-with-lustre-ui.html"
        False -> "index.html"
      })
      use config <- cli.do(cli.from_result(project.config(False)))
      let html = string.replace(html, "{app_name}", config.name)
      use _ <- cli.try(simplifile.write(index, html), fn(reason) {
        CannotWriteFile(reason, "index.html")
      })

      cli.return(Nil)
    }
  }
}
