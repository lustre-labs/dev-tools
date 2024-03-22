// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/bool
import gleam/dict
import gleam/io
import gleam/list
import gleam/package_interface.{type Type, Named, Variable}
import gleam/result
import gleam/string
import glint.{type Command, CommandInput}
import glint/flag
import lustre_dev_tools/esbuild
import lustre_dev_tools/project.{type Module}
import lustre_dev_tools/cli.{type Cli}
import lustre_dev_tools/tailwind
import simplifile

// COMMANDS --------------------------------------------------------------------

pub fn app() -> Command(Nil) {
  let description =
    "
Build and bundle an entire Lustre application. The generated JavaScript module
calls your app's `main` function on page load and can be included in any Web
page without Gleam or Lustre being present.

This is different from using `gleam build` directly because it produces a single
JavaScript module for you to host or distribute.
    "

  glint.command(fn(input) {
    let CommandInput(flags: flags, ..) = input
    let assert Ok(minify) = flag.get_bool(flags, "minify")

    let script = {
      use <- cli.log("Building your project")
      use project_name <- cli.do_result(get_project_name())
      use <- cli.success("Project compiled successfully")
      use <- cli.log("Checking if I can bundle your application")
      use module <- cli.do_result(get_module_interface(project_name))
      use _ <- cli.do_result(check_main_function(project_name, module))

      use <- cli.log("Creating the bundle entry file")
      let root = project.root()
      let tempdir = filepath.join(root, "build/.lustre")
      let outdir = filepath.join(root, "priv/static")
      let _ = simplifile.create_directory_all(tempdir)
      let _ = simplifile.create_directory_all(outdir)
      use template <- cli.template("entry-with-main.mjs", InternalError)
      let entry = string.replace(template, "{app_name}", project_name)

      let entryfile = filepath.join(tempdir, "entry.mjs")
      let ext = case minify {
        True -> ".min.mjs"
        False -> ".mjs"
      }

      let outfile =
        project_name
        |> string.append(ext)
        |> filepath.join(outdir, _)

      let assert Ok(_) = simplifile.write(entryfile, entry)

      use _ <- cli.do(bundle(entry, tempdir, outfile, minify))
      use entry <- cli.template("entry.css", InternalError)
      let outfile =
        filepath.strip_extension(outfile)
        |> string.append(".css")

      let bundle = bundle_tailwind(entry, tempdir, outfile, minify)
      use _ <- cli.do(cli.map_error(bundle, TailwindBundleError))

      cli.return(Nil)
    }

    case cli.run(script, Nil) {
      Ok(_) -> Nil
      Error(error) -> explain(error)
    }
  })
  |> glint.description(description)
  |> glint.unnamed_args(glint.EqArgs(0))
  |> glint.flag("minify", {
    let description = "Minify the output"
    let default = False

    flag.bool()
    |> flag.default(default)
    |> flag.description(description)
  })
}

pub fn component() -> Command(Nil) {
  let description =
    "
Build a Lustre component as a portable Web Component. The generated JavaScript
module can be included in any Web page and used without Gleam or Lustre being
present.
  "

  glint.command(fn(input) {
    let CommandInput(flags: flags, named_args: args, ..) = input
    let assert Ok(module_path) = dict.get(args, "module_path")
    let assert Ok(minify) = flag.get_bool(flags, "minify")

    let script = {
      use <- cli.log("Building your project")
      use module <- cli.do_result(get_module_interface(module_path))
      use <- cli.success("Project compiled successfully")
      use <- cli.log("Checking if I can bundle your component")
      use _ <- cli.do_result(check_component_name(module_path, module))
      use component <- cli.do_result(find_component(module_path, module))

      use <- cli.log("Creating the bundle entry file")
      let root = project.root()
      let tempdir = filepath.join(root, "build/.lustre")
      let outdir = filepath.join(root, "priv/static")
      let _ = simplifile.create_directory_all(tempdir)
      let _ = simplifile.create_directory_all(outdir)

      use project_name <- cli.do_result(get_project_name())

      // Esbuild bundling
      use template <- cli.template("component-entry.mjs", InternalError)
      let entry =
        template
        |> string.replace("{component_name}", component)
        |> string.replace("{app_name}", project_name)
        |> string.replace("{module_path}", module_path)

      let entryfile = filepath.join(tempdir, "entry.mjs")
      let ext = case minify {
        True -> ".min.mjs"
        False -> ".mjs"
      }
      let assert Ok(outfile) =
        string.split(module_path, "/")
        |> list.last
        |> result.map(string.append(_, ext))
        |> result.map(filepath.join(outdir, _))

      let assert Ok(_) = simplifile.write(entryfile, entry)
      use _ <- cli.do(bundle(entry, tempdir, outfile, minify))

      // Tailwind bundling
      use entry <- cli.template("entry.css", InternalError)
      let outfile =
        filepath.strip_extension(outfile)
        |> string.append(".css")

      let bundle = bundle_tailwind(entry, tempdir, outfile, minify)
      use _ <- cli.do(cli.map_error(bundle, TailwindBundleError))

      cli.return(Nil)
    }

    case cli.run(script, Nil) {
      Ok(_) -> Nil
      Error(error) -> explain(error)
    }
  })
  |> glint.description(description)
  |> glint.named_args(["module_path"])
  |> glint.unnamed_args(glint.EqArgs(0))
  |> glint.flag("minify", {
    let description = "Minify the output"
    let default = False

    flag.bool()
    |> flag.default(default)
    |> flag.description(description)
  })
}

// ERROR HANDLING --------------------------------------------------------------

type Error {
  BuildError
  BundleError(esbuild.Error)
  TailwindBundleError(tailwind.Error)
  ComponentMissing(module: String)
  MainMissing(module: String)
  ModuleMissing(module: String)
  NameIncorrectType(module: String, got: Type)
  NameMissing(module: String)
  InternalError(message: String)
}

fn explain(error: Error) -> Nil {
  case error {
    BuildError -> project.explain(project.BuildError)

    BundleError(error) -> esbuild.explain(error)

    TailwindBundleError(error) -> tailwind.explain(error)

    ComponentMissing(module) -> io.println("
Module `" <> module <> "` doesn't have any public function I can use to bundle
a component.

To bundle a component your module should have a public function that returns a
Lustre `App`:

  import lustre.{type App}
  pub fn my_component() -> App(flags, model, msg) {
    todo as \"your Lustre component to bundle\"
  }
")

    MainMissing(module) -> io.println("
Module `" <> module <> "` doesn't have a public `main` function I can use as
the bundle entry point.")

    ModuleMissing(module) -> io.println("
I couldn't find a public module called `" <> module <> "` in your project.")

    NameIncorrectType(module, type_) -> io.println("
I can't use the `name` constant exposed by module `" <> module <> "`
to give a name to the component I'm bundling.
I was expecting `name` to be a `String`,
but it has type `" <> project.type_to_string(type_) <> "`.")

    NameMissing(module) -> io.println("
Module `" <> module <> "` doesn't have a public `name` constant.
That is required so that I can give a proper name to the component I'm bundling.

Try adding a `name` constant to your module like this:

  const name: String = \"component-name\"")

    InternalError(message) -> io.println("
I ran into an error I wasn't expecting. Please open an issue on GitHub at
https://github.com/lustre-labs/cli with the following message:
" <> message)
  }
}

// STEPS -----------------------------------------------------------------------

fn get_project_name() -> Result(String, Error) {
  project.config()
  |> result.replace_error(BuildError)
  |> result.map(fn(confg) { confg.name })
}

fn get_module_interface(module_path: String) -> Result(Module, Error) {
  project.interface()
  |> result.replace_error(BuildError)
  |> result.then(fn(interface) {
    dict.get(interface.modules, module_path)
    |> result.replace_error(ModuleMissing(module_path))
  })
}

fn check_main_function(
  module_path: String,
  module: Module,
) -> Result(Nil, Error) {
  case dict.has_key(module.functions, "main") {
    True -> Ok(Nil)
    False -> Error(MainMissing(module_path))
  }
}

fn check_component_name(
  module_path: String,
  module: Module,
) -> Result(Nil, Error) {
  dict.get(module.constants, "name")
  |> result.replace_error(NameMissing(module_path))
  |> result.then(fn(component_name) {
    case is_string_type(component_name) {
      True -> Ok(Nil)
      False -> Error(NameIncorrectType(module_path, component_name))
    }
  })
}

fn find_component(module_path: String, module: Module) -> Result(String, Error) {
  let functions = dict.to_list(module.functions)
  let error = Error(ComponentMissing(module_path))

  use _, #(name, t) <- list.fold_until(functions, error)
  case t.parameters, is_compatible_app_type(t.return) {
    [], True -> list.Stop(Ok(name))
    _, _ -> list.Continue(error)
  }
}

fn bundle(
  entry: String,
  tempdir: String,
  outfile: String,
  minify: Bool,
) -> Cli(any, Nil, Error) {
  let entryfile = filepath.join(tempdir, "entry.mjs")
  let assert Ok(_) = simplifile.write(entryfile, entry)
  use _ <- cli.do(
    esbuild.bundle(entryfile, outfile, minify)
    |> cli.map_error(BundleError),
  )

  cli.return(Nil)
}

fn bundle_tailwind(
  entry: String,
  tempdir: String,
  outfile: String,
  minify: Bool,
) -> Cli(any, Nil, tailwind.Error) {
  // We first check if there's a `tailwind.config.js` at the project's root.
  // If not present we do nothing; otherwise we go on with bundling.
  let root = project.root()
  let tailwind_config_file = filepath.join(root, "tailwind.config.js")
  let has_tailwind_config =
    simplifile.verify_is_file(tailwind_config_file)
    |> result.unwrap(False)
  use <- bool.guard(when: !has_tailwind_config, return: cli.return(Nil))

  use _ <- cli.do(tailwind.setup(get_os(), get_cpu()))

  use <- cli.log("Bundling with Tailwind")
  let entryfile = filepath.join(tempdir, "entry.css")
  let assert Ok(_) = simplifile.write(entryfile, entry)

  let flags = ["--watch", "--input=" <> entryfile, "--output=" <> outfile]
  let options = case minify {
    True -> ["--minify", ..flags]
    False -> flags
  }
  use _ <- cli.try(
    cli.exec("./build/.lustre/bin/tailwind", in: root, with: options),
    fn(pair) { tailwind.BundleError(pair.1) },
  )
  use <- cli.success("Bundle produced at `" <> outfile <> "`")

  cli.return(Nil)
}

// UTILS -----------------------------------------------------------------------

fn is_string_type(t: Type) -> Bool {
  case t {
    Named(name: "String", package: "", module: "gleam", parameters: []) -> True
    _ -> False
  }
}

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

fn is_compatible_app_type(t: Type) -> Bool {
  case t {
    Named(
      name: "App",
      package: "lustre",
      module: "lustre",
      parameters: [flags, ..],
    ) -> is_nil_type(flags) || is_type_variable(flags)
    _ -> False
  }
}

// EXTERNALS -------------------------------------------------------------------

@external(erlang, "cli_ffi", "get_os")
fn get_os() -> String

@external(erlang, "cli_ffi", "get_cpu")
fn get_cpu() -> String
