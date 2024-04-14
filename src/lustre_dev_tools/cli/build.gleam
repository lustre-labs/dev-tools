// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/bool
import gleam/dict
import gleam/list
import gleam/package_interface.{type Type, Named, Variable}
import gleam/result
import gleam/string
import glint.{type Command, CommandInput}
import glint/flag
import lustre_dev_tools/cli.{type Cli}
import lustre_dev_tools/cmd
import lustre_dev_tools/error.{
  type Error, BundleError, ComponentMissing, MainMissing, ModuleMissing,
  NameIncorrectType, NameMissing,
}
import lustre_dev_tools/esbuild
import lustre_dev_tools/project.{type Module}
import lustre_dev_tools/tailwind
import simplifile

// DESCRIPTION -----------------------------------------------------------------
pub const description: String = "
Commands to build different kinds of Lustre application. These commands go beyond
just running `gleam build` and handle features like bundling, minification, and
integration with other build tools.
"

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
      do_app(minify)
    }

    case cli.run(script, flags) {
      Ok(_) -> Nil
      Error(error) -> error.explain(error)
    }
  })
  |> glint.description(description)
  |> glint.unnamed_args(glint.EqArgs(0))
  |> glint.flag("minify", {
    let description =
      "Minify the output, renaming variables and removing whitespace."
    let default = False

    flag.bool()
    |> flag.default(default)
    |> flag.description(description)
  })
}

pub fn do_app(minify: Bool) -> Cli(Nil) {
  use <- cli.log("Building your project")
  use project_name <- cli.try(get_project_name())

  use <- cli.success("Project compiled successfully")
  use <- cli.log("Checking if I can bundle your application")
  use module <- cli.try(get_module_interface(project_name))
  use _ <- cli.try(check_main_function(project_name, module))

  use <- cli.log("Creating the bundle entry file")
  let root = project.root()
  let tempdir = filepath.join(root, "build/.lustre")
  let outdir = filepath.join(root, "priv/static")
  let _ = simplifile.create_directory_all(tempdir)
  let _ = simplifile.create_directory_all(outdir)
  use template <- cli.template("entry-with-main.mjs")
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
  use entry <- cli.template("entry.css")
  let outfile =
    filepath.strip_extension(outfile)
    |> string.append(".css")

  use _ <- cli.do(bundle_tailwind(entry, tempdir, outfile, minify))

  cli.return(Nil)
}

pub fn component() -> Command(Nil) {
  let description =
    "
Build a Lustre component as a portable Web Component. The generated JavaScript
module can be included in any Web page and used without Gleam or Lustre being
present.

For a module to be built as a component, it must expose a `name` constant that
will be the name of the component's HTML tag, and contain a public function that
returns a suitable Lustre `App`.
  "

  glint.command(fn(input) {
    let CommandInput(flags: flags, named_args: args, ..) = input
    let assert Ok(module_path) = dict.get(args, "module_path")
    let assert Ok(minify) = flag.get_bool(flags, "minify")

    let script = {
      use <- cli.log("Building your project")
      use module <- cli.try(get_module_interface(module_path))
      use <- cli.success("Project compiled successfully")
      use <- cli.log("Checking if I can bundle your component")
      use _ <- cli.try(check_component_name(module_path, module))
      use component <- cli.try(find_component(module_path, module))

      use <- cli.log("Creating the bundle entry file")
      let root = project.root()
      let tempdir = filepath.join(root, "build/.lustre")
      let outdir = filepath.join(root, "priv/static")
      let _ = simplifile.create_directory_all(tempdir)
      let _ = simplifile.create_directory_all(outdir)

      use project_name <- cli.try(get_project_name())

      // Esbuild bundling
      use template <- cli.template("component-entry.mjs")
      let entry =
        template
        |> string.replace("{component_name}", importable_name(component))
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
      use entry <- cli.template("entry.css")
      let outfile =
        filepath.strip_extension(outfile)
        |> string.append(".css")

      use _ <- cli.do(bundle_tailwind(entry, tempdir, outfile, minify))

      cli.return(Nil)
    }

    case cli.run(script, flags) {
      Ok(_) -> Nil
      Error(error) -> error.explain(error)
    }
  })
  |> glint.description(description)
  |> glint.named_args(["module_path"])
  |> glint.unnamed_args(glint.EqArgs(0))
  |> glint.flag("minify", {
    let description =
      "Minify the output, renaming variables and removing whitespace."
    let default = False

    flag.bool()
    |> flag.default(default)
    |> flag.description(description)
  })
}

// STEPS -----------------------------------------------------------------------

fn get_project_name() -> Result(String, Error) {
  project.config(False)
  |> result.map(fn(confg) { confg.name })
}

fn get_module_interface(module_path: String) -> Result(Module, Error) {
  project.interface()
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
) -> Cli(Nil) {
  let entryfile = filepath.join(tempdir, "entry.mjs")
  let assert Ok(_) = simplifile.write(entryfile, entry)
  use _ <- cli.do(esbuild.bundle(entryfile, outfile, minify))

  cli.return(Nil)
}

fn bundle_tailwind(
  entry: String,
  tempdir: String,
  outfile: String,
  minify: Bool,
) -> Cli(Nil) {
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
  cmd.exec("./build/.lustre/bin/tailwind", in: root, with: options)
  |> result.map_error(fn(pair) { BundleError(pair.1) })
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

/// Turns a Gleam identifier into a name that can be imported in an mjs module
/// from Gleam's generated code.
///
fn importable_name(identifier: String) -> String {
  case is_reserved_keyword(identifier) {
    True -> identifier <> "$"
    False -> identifier
  }
}

fn is_reserved_keyword(name: String) -> Bool {
  // This list is taken directly from Gleam's compiler: there's some identifiers
  // that are not technically keywords (like `then`) but Gleam will still append
  // a "$" to those.
  case name {
    "await"
    | "arguments"
    | "break"
    | "case"
    | "catch"
    | "class"
    | "const"
    | "continue"
    | "debugger"
    | "default"
    | "delete"
    | "do"
    | "else"
    | "enum"
    | "export"
    | "extends"
    | "eval"
    | "false"
    | "finally"
    | "for"
    | "function"
    | "if"
    | "implements"
    | "import"
    | "in"
    | "instanceof"
    | "interface"
    | "let"
    | "new"
    | "null"
    | "package"
    | "private"
    | "protected"
    | "public"
    | "return"
    | "static"
    | "super"
    | "switch"
    | "this"
    | "throw"
    | "true"
    | "try"
    | "typeof"
    | "var"
    | "void"
    | "while"
    | "with"
    | "yield"
    | "undefined"
    | "then" -> True
    _ -> False
  }
}

// EXTERNALS -------------------------------------------------------------------

@external(erlang, "lustre_dev_tools_ffi", "get_os")
fn get_os() -> String

@external(erlang, "lustre_dev_tools_ffi", "get_cpu")
fn get_cpu() -> String
