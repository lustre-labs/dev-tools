// IMPORTS ---------------------------------------------------------------------

import glint.{type Command, CommandInput}
import glint/flag
import glint/flag/constraint
import lustre_dev_tools/esbuild
import lustre_dev_tools/cli
import lustre_dev_tools/tailwind

// DESCRIPTION -----------------------------------------------------------------

pub const description: String = "
Commands for adding external binaries to your project. These are run and managed
by Lustre, and while not typically intended to be run manually, they can be found
inside `build/.lustre/bin`.
  "

// COMMANDS --------------------------------------------------------------------

pub fn esbuild() -> Command(Nil) {
  let description =
    "
Download a platform-appropriate version of the esbuild binary. Lustre uses this
to bundle applications and act as a development server, and will automatically
download the binary if either the `build` or `start` commands are run.
    "

  glint.command(fn(input) {
    let CommandInput(flags: flags, ..) = input
    let assert Ok(os) = flag.get_string(flags, "os")
    let assert Ok(cpu) = flag.get_string(flags, "cpu")
    let script = esbuild.download(os, cpu)

    case cli.run(script, Nil) {
      Ok(_) -> Nil
      Error(error) -> esbuild.explain(error)
    }
  })
  |> glint.description(description)
  |> glint.unnamed_args(glint.EqArgs(0))
  |> glint.flag("os", {
    let description = "Override the automatic OS detection."
    let default = get_os()
    let allowed = [
      "android", "darwin", "freebsd", "linux", "win32", "netbsd", "openbsd",
      "sunos",
    ]

    flag.string()
    |> flag.default(default)
    |> flag.description(description)
    |> flag.constraint(constraint.one_of(allowed))
  })
  |> glint.flag("cpu", {
    let description = "Override the automatic CPU architecture detection."
    let default = get_cpu()
    let allowed = ["aarch64", "amd64", "arm", "arm64", "ia32", "x64", "x86_64"]

    flag.string()
    |> flag.default(default)
    |> flag.description(description)
    |> flag.constraint(constraint.one_of(allowed))
  })
}

pub fn tailwind() -> Command(Nil) {
  let description =
    "
Download a platform-appropriate version of the Tailwind binary. Lustre will
automatically use this to compile your styles if it detects a `tailwind.config.js`
in your project but will not download it automatically.
    "

  glint.command(fn(input) {
    let CommandInput(flags: flags, ..) = input
    let assert Ok(os) = flag.get_string(flags, "os")
    let assert Ok(cpu) = flag.get_string(flags, "cpu")
    let script = tailwind.setup(os, cpu)

    case cli.run(script, Nil) {
      Ok(_) -> Nil
      Error(error) -> tailwind.explain(error)
    }
  })
  |> glint.description(description)
  |> glint.unnamed_args(glint.EqArgs(0))
  |> glint.flag("os", {
    let description = "Override the automatic OS detection."
    let default = get_os()
    let allowed = ["linux", "win32", "darwin"]

    flag.string()
    |> flag.default(default)
    |> flag.description(description)
    |> flag.constraint(constraint.one_of(allowed))
  })
  |> glint.flag("cpu", {
    let description = "Override the automatic CPU architecture detection."
    let default = get_cpu()
    let allowed = ["armv7", "arm64", "x64", "x86_64", "aarch64"]

    flag.string()
    |> flag.default(default)
    |> flag.description(description)
    |> flag.constraint(constraint.one_of(allowed))
  })
}

// EXTERNALS -------------------------------------------------------------------

@external(erlang, "lustre_dev_tools_ffi", "get_os")
fn get_os() -> String

@external(erlang, "lustre_dev_tools_ffi", "get_cpu")
fn get_cpu() -> String
