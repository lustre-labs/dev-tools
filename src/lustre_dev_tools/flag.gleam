// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/result
import glint/flag.{type Flag}
import lustre_dev_tools/project.{type Config}
import tom

// ACCESSORS -------------------------------------------------------------------

pub fn get_string(
  name: String,
  default: String,
  flags: Dict(String, Flag),
  config: Config,
) -> String {
  result.or(
    result.nil_error(flag.get_string(flags, name)),
    result.nil_error(tom.get_string(config.toml, ["lustre", "dev", name])),
  )
  |> result.unwrap(default)
}

pub fn get_bool(
  name: String,
  default: Bool,
  flags: Dict(String, Flag),
  config: Config,
) -> Bool {
  result.or(
    result.nil_error(flag.get_bool(flags, name)),
    result.nil_error(tom.get_bool(config.toml, ["lustre", "dev", name])),
  )
  |> result.unwrap(default)
}

pub fn get_int(
  name: String,
  default: Int,
  flags: Dict(String, Flag),
  config: Config,
) -> Int {
  result.or(
    result.nil_error(flag.get_int(flags, name)),
    result.nil_error(tom.get_int(config.toml, ["lustre", "dev", name])),
  )
  |> result.unwrap(default)
}
