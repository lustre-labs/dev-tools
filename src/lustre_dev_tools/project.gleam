// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/dict.{type Dict}
import simplifile
import tom.{type Toml}

//

pub type Meta {
  Meta(root: String, src: String, name: String, toml: Dict(String, Toml))
}

///
///
pub fn root() -> String {
  do_root(".")
}

fn do_root(path: String) -> String {
  let toml = filepath.join(path, "gleam.toml")

  case simplifile.is_file(toml) {
    Ok(True) -> path
    Ok(False) | Error(_) -> do_root(filepath.join("..", path))
  }
}

///
///
pub fn meta() -> Meta {
  let root = root()
  let src = filepath.join(root, "src")

  // These are all safe to assert because a user *must* be in a valid Gleam project
  // if they are running the program, and a valid Gleam project *must* have a
  // `gleam.toml` with at least a `name` field.
  let assert Ok(toml) = simplifile.read(filepath.join(root, "gleam.toml"))
  let assert Ok(toml) = tom.parse(toml)
  let assert Ok(name) = tom.get_string(toml, ["name"])

  Meta(root:, src:, name:, toml:)
}
