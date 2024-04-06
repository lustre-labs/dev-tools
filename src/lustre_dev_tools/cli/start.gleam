// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/dict
import gleam/package_interface.{type Type, Fn, Named, Variable}
import gleam/result
import gleam/string
import glint.{type Command, CommandInput}
import glint/flag
import lustre_dev_tools/error.{
  type Error, InternalError, MainBadAppType, MainMissing, MainTakesAnArgument,
  ModuleMissing, SimplifileError,
}
import lustre_dev_tools/cli
import lustre_dev_tools/esbuild
import lustre_dev_tools/project.{type Module}
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
    let assert Ok(host) = flag.get_string(flags, "host")
    let assert Ok(port) = flag.get_string(flags, "port")
    let assert Ok(use_example_styles) =
      flag.get_bool(flags, "use-example-styles")
    let assert Ok(no_spa) = flag.get_bool(flags, "no-spa")
    let custom_html = flag.get_string(flags, "html")

    let script = {
      use <- cli.log("Building your project")
      use interface <- cli.do(cli.from_result(project.interface()))
      use module <- cli.try(dict.get(interface.modules, interface.name), fn(_) {
        ModuleMissing(interface.name)
      })
      use is_app <- cli.do_result(check_is_lustre_app(interface.name, module))
      use <- cli.success("Project compiled successfully")

      use <- cli.log("Creating the application entry point")
      let root = project.root()
      let tempdir = filepath.join(root, "build/.lustre")
      let _ = simplifile.create_directory_all(tempdir)
      use template <- cli.template(
        case is_app {
          True -> "entry-with-start.mjs"
          False -> "entry-with-main.mjs"
        },
        InternalError,
      )

      let entry = string.replace(template, "{app_name}", interface.name)
      use html <- cli.do(case custom_html {
        Ok(custom_html_path) ->
          custom_html_path
          |> simplifile.read
          |> result.map_error(SimplifileError(_, custom_html_path))
          |> result.map(string.replace(
            _,
            "<script type=\"application/lustre\">",
            "<script type=\"module\" src=\"./index.mjs\">",
          ))
          |> cli.from_result

        Error(_) if use_example_styles -> {
          let name = "index-with-lustre-ui.html"
          use template <- cli.template(name, InternalError)
          let html = string.replace(template, "{app_name}", interface.name)

          cli.return(html)
        }

        _ -> {
          let name = "index.html"
          use template <- cli.template(name, InternalError)
          let html = string.replace(template, "{app_name}", interface.name)

          cli.return(html)
        }
      })

      let assert Ok(_) = simplifile.write(tempdir <> "/entry.mjs", entry)
      let assert Ok(_) = simplifile.write(tempdir <> "/index.html", html)

      use _ <- cli.do(esbuild.bundle(
        filepath.join(tempdir, "entry.mjs"),
        filepath.join(tempdir, "index.mjs"),
        False,
      ))

      use _ <- cli.do(esbuild.serve(host, port, !no_spa))

      cli.return(Nil)
    }

    case cli.run(script, Nil) {
      Ok(_) -> Nil
      Error(error) -> error.explain(error)
    }
  })
  |> glint.description(description)
  |> glint.unnamed_args(glint.EqArgs(0))
  |> glint.flag("host", {
    let description =
      "Specify which IP addresses the server should listen on. Set this to `0.0.0.0` to listen on all addresses, including LAN and public addresses."
    let default = "localhost"

    flag.string()
    |> flag.default(default)
    |> flag.description(description)
  })
  |> glint.flag("port", {
    let description =
      "Specify server port. If the port is taken the dev server will not start."
    let default = "1234"

    flag.string()
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
  |> glint.flag("no-spa", {
    let description =
      "The dev server serves your app on every route to make developing a SPA easier. The --no-spa flag tells the dev server to 404 on any path other than `/`."
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

fn check_is_lustre_app(
  module_path: String,
  module: Module,
) -> Result(Bool, Error) {
  dict.get(module.functions, "main")
  |> result.replace_error(MainMissing(module_path))
  |> result.then(fn(main) {
    case main.parameters, main.return {
      [_, ..], _ ->
        Error(MainTakesAnArgument(module_path, Fn(main.parameters, main.return)))

      [], Named(
        name: "App",
        package: "lustre",
        module: "lustre",
        parameters: [flags, model, msg],
      ) ->
        case is_compatible_flags_type(flags) {
          True -> Ok(True)
          False -> Error(MainBadAppType(module_path, flags, model, msg))
        }

      [], _ -> Ok(False)
    }
  })
}

// UTILS -----------------------------------------------------------------------

fn is_nil_type(t: Type) -> Bool {
  case t {
    Named(name: "Nil", package: "", module: "gleam", parameters: []) -> True
    _ -> False
  }
}

fn is_type_variable(t: Type) -> Bool {
  case t {
    Variable(..) -> True
    _ -> False
  }
}

fn is_compatible_flags_type(t: Type) -> Bool {
  is_nil_type(t) || is_type_variable(t)
}
