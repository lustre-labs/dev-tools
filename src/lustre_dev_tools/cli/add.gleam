// IMPORTS ---------------------------------------------------------------------

import gleam/io
import glint.{type Command}
import lustre_dev_tools/cli.{do}
import lustre_dev_tools/cli/flag
import lustre_dev_tools/error
import lustre_dev_tools/esbuild
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

  use <- glint.command_help(description)
  use <- glint.unnamed_args(glint.EqArgs(0))
  use os <- glint.flag(flag.esbuild_os())
  use cpu <- glint.flag(flag.esbuild_cpu())
  use _, _, flags <- glint.command()
  let script = {
    use os <- do(cli.get_string("os", get_os(), ["add"], os))
    use cpu <- do(cli.get_string("cpu", get_cpu(), ["add"], cpu))

    esbuild.download(os, cpu)
  }

  case cli.run(script, flags) {
    Ok(_) -> Nil
    Error(error) -> error.explain(error) |> io.print_error
  }
}

pub fn tailwind() -> Command(Nil) {
  let description =
    "
Download a platform-appropriate version of the Tailwind binary. Lustre will
automatically use this to compile your styles if it detects a `tailwind.config.js`
in your project but will not download it automatically.
    "
  use <- glint.command_help(description)
  use <- glint.unnamed_args(glint.EqArgs(0))
  use os <- glint.flag(flag.tailwind_os())
  use cpu <- glint.flag(flag.tailwind_cpu())
  use _, _, flags <- glint.command()
  let script = {
    use os <- do(cli.get_string("os", get_os(), ["add"], os))
    use cpu <- do(cli.get_string("cpu", get_cpu(), ["add"], cpu))

    tailwind.setup(os, cpu)
  }

  case cli.run(script, flags) {
    Ok(_) -> Nil
    Error(error) -> error.explain(error) |> io.print_error
  }
}

// EXTERNALS -------------------------------------------------------------------

@external(erlang, "lustre_dev_tools_ffi", "get_os")
fn get_os() -> String

@external(erlang, "lustre_dev_tools_ffi", "get_cpu")
fn get_cpu() -> String
