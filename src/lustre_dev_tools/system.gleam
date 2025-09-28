///
///
@external(erlang, "system_ffi", "detect_os")
pub fn detect_os() -> String

///
///
@external(erlang, "system_ffi", "detect_arch")
pub fn detect_arch() -> String

///
///
@external(erlang, "system_ffi", "is_alpine")
pub fn is_alpine() -> Bool

/// Run a system command and return the output as a string. If the command exits
/// with a non-zero status, any output will be returned as an `Error` instead.
///
@external(erlang, "system_ffi", "run")
pub fn run(command: String) -> Result(String, String)

@external(erlang, "system_ffi", "find")
pub fn find(executable: String) -> Result(String, Nil)

///
///
@external(erlang, "system_ffi", "cwd")
pub fn cwd() -> String

///
///
@external(erlang, "system_ffi", "exit")
pub fn exit(code: Int) -> Nil
