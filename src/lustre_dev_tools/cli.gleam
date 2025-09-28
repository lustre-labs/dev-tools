// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/function
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import gleam_community/ansi
import glint
import lustre_dev_tools/error.{type Error}
import lustre_dev_tools/project.{type Project}
import tom.{type GetError, type Toml}

//

pub fn log(message: String, quiet: Bool) -> Nil {
  case quiet {
    True -> Nil
    False -> io.println(ansi.grey("   " <> message <> "..."))
  }
}

pub fn success(message: String, quiet: Bool) -> Nil {
  case quiet {
    True -> Nil
    False -> io.println(ansi.green("✅ " <> message))
  }
}

///
///
pub fn bool(
  name: String,
  path: List(String),
  project: Project,
  description: String,
  continue: fn(fn(glint.Flags) -> Result(Bool, Error)) -> glint.Command(b),
) -> glint.Command(b) {
  flag(
    name,
    path,
    project,
    glint.bool_flag,
    tom.get_bool,
    description,
    continue,
  )
}

///
///
pub fn string(
  name: String,
  path: List(String),
  project: Project,
  description: String,
  continue: fn(fn(glint.Flags) -> Result(String, Error)) -> glint.Command(b),
) -> glint.Command(b) {
  flag(
    name,
    path,
    project,
    glint.string_flag,
    tom.get_string,
    description,
    continue,
  )
}

pub fn string_list(
  name: String,
  path: List(String),
  project: Project,
  description: String,
  continue: fn(fn(glint.Flags) -> Result(List(String), Error)) ->
    glint.Command(b),
) -> glint.Command(b) {
  let get_strings = fn(toml, path) {
    tom.get_array(toml, path)
    |> result.try(list.try_map(_, tom.as_string))
  }

  flag(
    name,
    path,
    project,
    glint.strings_flag,
    get_strings,
    description,
    continue,
  )
}

///
///
pub fn int(
  name: String,
  path: List(String),
  project: Project,
  description: String,
  continue: fn(fn(glint.Flags) -> Result(Int, Error)) -> glint.Command(b),
) -> glint.Command(b) {
  flag(name, path, project, glint.int_flag, tom.get_int, description, continue)
}

///
///
fn flag(
  name: String,
  path: List(String),
  project: Project,
  read_flag: fn(String) -> glint.Flag(a),
  read_toml: fn(Dict(String, Toml), List(String)) -> Result(a, GetError),
  description: String,
  continue: fn(fn(glint.Flags) -> Result(a, Error)) -> glint.Command(b),
) -> glint.Command(b) {
  let flag = fn(next) {
    use from_flags <- glint.flag(
      read_flag(name) |> glint.flag_help(description |> string.trim),
    )

    use flags <- continue
    let result = case from_flags(flags) {
      Ok(value) -> Ok(value)
      Error(_) -> Error(error.MissingRequiredFlag(name: path))
    }

    next(result)
  }

  let option = fn() {
    case read_toml(project.options, path) {
      Ok(value) -> Ok(value)
      Error(_) -> Error(error.MissingRequiredFlag(name: path))
    }
  }

  case name, path {
    "", [] ->
      continue(fn(_) {
        panic as "Invalid flag config. Please open an issue at https://github.com/lustre-labs/dev-tools/issues/new"
      })
    "", _ -> continue(fn(_) { option() })
    _, [] -> flag(function.identity)
    _, _ -> flag(fn(_) { option() })
  }
}
