import glint
import glint/constraint

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

pub fn tailwind_os() -> glint.Flag(String) {
  let description = "Override the automatic OS detection."
  let allowed = ["linux", "win32", "darwin"]

  glint.string_flag("os")
  |> glint.flag_help(description)
  |> glint.flag_constraint(constraint.one_of(allowed))
}

pub fn tailwind_cpu() -> glint.Flag(String) {
  let description = "Override the automatic CPU architecture detection."
  let allowed = ["arm64", "x64", "x86_64", "aarch64"]

  glint.string_flag("cpu")
  |> glint.flag_help(description)
  |> glint.flag_constraint(constraint.one_of(allowed))
}

pub fn minify() -> glint.Flag(Bool) {
  let description =
    "Minify the output, renaming variables and removing whitespace."

  glint.bool_flag("minify")
  |> glint.flag_help(description)
}

pub fn tailwind_entry() -> glint.Flag(String) {
  let description =
    "Use a custom CSS file as the entry to a Tailwind CSS bundle."

  glint.string_flag("tailwind-entry")
  |> glint.flag_help(description)
}

pub fn outdir() -> glint.Flag(String) {
  let description =
    "Use a custom directory as the destination for any built files."

  glint.string_flag("outdir")
  |> glint.flag_help(description)
}

pub fn ext() -> glint.Flag(String) {
  let description =
    "Use a file extension other than 'mjs' for the built JavaScript."

  glint.string_flag("ext")
  |> glint.flag_help(description)
}

pub fn detect_tailwind() -> glint.Flag(Bool) {
  let description = "Detect and build Tailwind styles automatically."

  glint.bool_flag("detect-tailwind")
  |> glint.flag_help(description)
}

pub fn port() -> glint.Flag(Int) {
  let description =
    "Specify server port. If the port is taken the dev server will not start."

  glint.int_flag("port")
  |> glint.flag_help(description)
}

pub fn bind() -> glint.Flag(String) {
  let description =
    "Specify server interface binding. If the provided interface is not valid or unavailable, the dev server will not start."

  glint.string_flag("bind")
  |> glint.flag_help(description)
}

pub fn proxy_from() -> glint.Flag(String) {
  let description =
    "Proxy requests that start with this path to the URL specified by the --proxy-to flag."

  glint.string_flag("proxy-from")
  |> glint.flag_help(description)
}

pub fn proxy_to() -> glint.Flag(String) {
  let description =
    "Proxy requests that start with the path specified by the --proxy-from flag to this URL."

  glint.string_flag("proxy-to")
  |> glint.flag_help(description)
}

pub fn entry() -> glint.Flag(String) {
  let description = "Specify an entry other than your app's main module."

  glint.string_flag("entry")
  |> glint.flag_help(description)
}
