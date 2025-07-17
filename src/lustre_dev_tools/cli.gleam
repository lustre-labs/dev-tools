// IMPORTS ---------------------------------------------------------------------

import gleam/string
import glint
import lustre_dev_tools/error.{type Error}
import lustre_dev_tools/project.{type Project}
import tom.{type GetError}

//

pub type Option(a) =
  #(fn(String) -> glint.Flag(a), fn(String) -> Result(a, GetError))

//

///
///
pub fn bool(project: Project) -> Option(Bool) {
  #(glint.bool_flag, project.get_bool)
}

///
///
pub fn string(project: Project) -> Option(String) {
  #(glint.string_flag, project.get_string)
}

///
///
pub fn int(project: Project) -> Option(Int) {
  #(glint.int_flag, project.get_int)
}

///
///
pub fn flag(
  name: String,
  read: Option(a),
  description: String,
  continue: fn(fn(glint.Flags) -> Result(a, Error)) -> glint.Command(b),
) -> glint.Command(b) {
  use read_flag <- glint.flag(
    read.0(name) |> glint.flag_help(description |> string.trim),
  )

  use flags <- continue

  case read_flag(flags) {
    Ok(value) -> Ok(value)
    Error(_) ->
      case read.1(name) {
        Ok(value) -> Ok(value)
        Error(_) -> Error(error.MissingRequiredFlag(name:))
      }
  }
}
