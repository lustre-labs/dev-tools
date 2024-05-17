import glint
import glint/constraint

pub fn minify() -> glint.Flag(Bool) {
  let description =
    "Minify the output, renaming variables and removing whitespace."

  glint.bool_flag("minify")
  |> glint.flag_help(description)
}

pub fn port() -> glint.Flag(Int) {
  let description =
    "Specify server port. If the port is taken the dev server will not start."

  glint.int_flag("port")
  |> glint.flag_help(description)
}

pub fn proxy_to() -> glint.Flag(String) {
  let description =
    "Proxy requests that start with the path specified by the --proxy-from flag to this URL."

  glint.string_flag("proxy-to")
  |> glint.flag_help(description)
}

pub fn proxy_from() -> glint.Flag(String) {
  let description =
    "Proxy requests that start with this path to the URL specified by the --proxy-to flag."

  glint.string_flag("proxy-from")
  |> glint.flag_help(description)
}

pub fn esbuild_os() -> glint.Flag(String) {
  let description = "Override the automatic OS detection."
  let allowed = [
    "android", "darwin", "freebsd", "linux", "win32", "netbsd", "openbsd",
    "sunos",
  ]

  glint.string_flag("os")
  |> glint.flag_help(description)
  |> glint.flag_constraint(constraint.one_of(allowed))
}

pub fn esbuild_cpu() -> glint.Flag(String) {
  let description = "Override the automatic CPU architecture detection."
  let allowed = ["aarch64", "amd64", "arm", "arm64", "ia32", "x64", "x86_64"]

  glint.string_flag("cpu")
  |> glint.flag_help(description)
  |> glint.flag_constraint(constraint.one_of(allowed))
}

@external(erlang, "lustre_dev_tools_ffi", "get_os")
fn get_os() -> String

pub fn tailwind_os() -> glint.Flag(String) {
  let description = "Override the automatic OS detection."
  let default = get_os()
  let allowed = ["linux", "win32", "darwin"]

  glint.string_flag("os")
  |> glint.flag_default(default)
  |> glint.flag_help(description)
  |> glint.flag_constraint(constraint.one_of(allowed))
}

@external(erlang, "lustre_dev_tools_ffi", "get_cpu")
fn get_cpu() -> String

pub fn tailwind_cpu() -> glint.Flag(String) {
  let description = "Override the automatic CPU architecture detection."
  let default = get_cpu()
  let allowed = ["armv7", "arm64", "x64", "x86_64", "aarch64"]

  glint.string_flag("cpu")
  |> glint.flag_default(default)
  |> glint.flag_help(description)
  |> glint.flag_constraint(constraint.one_of(allowed))
}
