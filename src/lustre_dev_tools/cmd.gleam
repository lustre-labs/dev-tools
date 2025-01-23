// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Dynamic}

// EXTERNALS -------------------------------------------------------------------

///
///
@external(erlang, "lustre_dev_tools_ffi", "exec")
pub fn exec(
  run command: String,
  with args: List(String),
  in in: String,
) -> Result(String, #(Int, String))

///
///
@external(erlang, "lustre_dev_tools_ffi", "get_cwd")
pub fn cwd() -> Result(String, Dynamic)

///
///
@external(erlang, "lustre_dev_tools_ffi", "find_executable")
pub fn find_executable(command: String) -> Result(String, Nil)
