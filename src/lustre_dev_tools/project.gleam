// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/dict.{type Dict}
import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/json
import gleam/list
import gleam/package_interface.{type Type, Fn, Named, Tuple, Variable}
import gleam/pair
import gleam/result
import gleam/string
import lustre_dev_tools/cmd
import lustre_dev_tools/error.{type Error, BuildError}
import simplifile
import tom.{type Toml}

// TYPES -----------------------------------------------------------------------

pub type Config {
  Config(name: String, toml: Dict(String, Toml))
}

pub type Interface {
  Interface(name: String, version: String, modules: Dict(String, Module))
}

pub type Module {
  Module(constants: Dict(String, Type), functions: Dict(String, Function))
}

pub type Function {
  Function(parameters: List(Type), return: Type)
}

// COMMANDS --------------------------------------------------------------------

pub fn otp_version() -> Int {
  let version = do_otp_version()
  case int.parse(version) {
    Ok(version) -> version
    Error(_) -> panic as { "unexpected version number format: " <> version }
  }
}

@external(erlang, "lustre_dev_tools_ffi", "otp_version")
fn do_otp_version() -> String

/// Compile the current project running the `gleam build` command.
///
pub fn build() -> Result(Nil, Error) {
  cmd.exec(run: "gleam", in: ".", env: [], with: [
    "build", "--target", "javascript",
  ])
  |> result.map_error(fn(err) { BuildError(pair.second(err)) })
  |> result.replace(Nil)
}

pub fn interface() -> Result(Interface, Error) {
  let dir = filepath.join(root(), "build/.lustre")
  let out = filepath.join(dir, "package-interface.json")
  let args = ["export", "package-interface", "--out", out]

  use _ <- result.try(
    cmd.exec(run: "gleam", in: ".", env: [], with: args)
    |> result.map_error(fn(err) { BuildError(pair.second(err)) }),
  )

  let assert Ok(json) = simplifile.read(out)
  let assert Ok(interface) = json.parse(json, interface_decoder())

  Ok(interface)
}

/// Read the project configuration in the `gleam.toml` file.
///
pub fn config() -> Result(Config, Error) {
  // Since we made sure that the project could compile we're sure that there is
  // bound to be a `gleam.toml` file somewhere in the current directory (or in
  // its parent directories). So we can safely call `root()` without
  // it looping indefinitely.
  let configuration_path = filepath.join(root(), "gleam.toml")

  // All these operations are safe to assert because the Gleam project wouldn't
  // compile if any of this stuff was invalid.
  let assert Ok(configuration) = simplifile.read(configuration_path)
  let assert Ok(toml) = tom.parse(configuration)
  let assert Ok(name) = tom.get_string(toml, ["name"])

  Ok(Config(name: name, toml: toml))
}

// UTILS -----------------------------------------------------------------------

/// Finds the path leading to the project's root folder. This recursively walks
/// up from the current directory until it finds a `gleam.toml`.
///
pub fn root() -> String {
  find_root(".")
}

fn find_root(path: String) -> String {
  let toml = filepath.join(path, "gleam.toml")

  case simplifile.is_file(toml) {
    Ok(False) | Error(_) -> find_root(filepath.join("..", path))
    Ok(True) -> path
  }
}

pub fn type_to_string(type_: Type) -> String {
  case type_ {
    Tuple(elements) -> {
      let elements = list.map(elements, type_to_string)
      "#(" <> string.join(elements, with: ", ") <> ")"
    }

    Fn(params, return) -> {
      let params = list.map(params, type_to_string)
      let return = type_to_string(return)
      "fn(" <> string.join(params, with: ", ") <> ") -> " <> return
    }

    Named(name, _package, _module, []) -> name
    Named(name, _package, _module, params) -> {
      let params = list.map(params, type_to_string)
      name <> "(" <> string.join(params, with: ", ") <> ")"
    }

    Variable(id) -> "a_" <> int.to_string(id)
  }
}

// DECODERS --------------------------------------------------------------------

fn interface_decoder() -> Decoder(Interface) {
  use name <- decode.field("name", decode.string)
  use version <- decode.field("version", decode.string)
  use modules <- decode.field("modules", string_dict(module_decoder()))

  decode.success(Interface(name:, version:, modules:))
}

fn module_decoder() -> Decoder(Module) {
  use constants <- decode.field(
    "constants",
    string_dict(decode.at(["type"], package_interface.type_decoder())),
  )
  use functions <- decode.field("functions", string_dict(function_decoder()))

  decode.success(Module(constants:, functions:))
}

fn function_decoder() -> Decoder(Function) {
  use parameters <- decode.field(
    "parameters",
    decode.list(labelled_argument_decoder()),
  )
  use return <- decode.field("return", package_interface.type_decoder())

  decode.success(Function(parameters:, return:))
}

fn labelled_argument_decoder() -> Decoder(Type) {
  // In this case we don't really care about the label, so we're just ignoring
  // it and returning the argument's type.
  decode.at(["type"], package_interface.type_decoder())
}

fn string_dict(values: Decoder(a)) -> Decoder(Dict(String, a)) {
  decode.dict(decode.string, values)
}
