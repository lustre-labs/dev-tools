// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
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
  case name, path {
    "", [] ->
      panic as "Invalid flag config. Please open an issue at https://github.com/lustre-labs/dev-tools/issues/new"

    // There is no CLI flag for this option, so we must read it from the config
    // file.
    "", _ ->
      continue(fn(_) {
        case read_toml(project.options, path) {
          Ok(value) -> Ok(value)
          Error(_) -> Error(error.MissingRequiredFlag(name: path))
        }
      })

    // There is no config option for this flag, so we must only read it from
    // the CLI args.
    _, [] -> {
      use from_flags <- glint.flag(
        read_flag(name) |> glint.flag_help(description |> string.trim),
      )

      use flags <- continue

      case from_flags(flags) {
        Ok(value) -> Ok(value)
        Error(_) -> Error(error.MissingRequiredFlag(name: ["--" <> name]))
      }
    }

    // This option could be read from both the config file and the CLI args.
    // Precedence is given to flags passed via the CLI, but the config file is
    // read as a fallback if the flag is not present.
    _, _ -> {
      use from_flags <- glint.flag(
        read_flag(name) |> glint.flag_help(description |> string.trim),
      )

      use flags <- continue

      case from_flags(flags) {
        Ok(value) -> Ok(value)
        Error(_) ->
          case read_toml(project.options, path) {
            Ok(value) -> Ok(value)
            Error(_) -> Error(error.MissingRequiredFlag(name: path))
          }
      }
    }
  }
}
