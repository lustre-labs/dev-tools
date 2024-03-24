// IMPORTS ---------------------------------------------------------------------

import argv
import glint
import lustre_dev_tools/cli/add
import lustre_dev_tools/cli/build
import lustre_dev_tools/cli/start

// MAIN ------------------------------------------------------------------------

/// The `main` function is used as the entry point for Lustre's dev tools. You
/// shouldn't run this function in your code, but instead use `Gleam run` to run
/// this module from the command line. To see what the dev tools can do, run:
///
/// ```
/// gleam run -m lustre/dev -- --help
/// ```
///
pub fn main() {
  let args = argv.load().arguments

  glint.new()
  |> glint.as_gleam_module
  |> glint.with_name("lustre/dev")
  |> glint.add(
    at: ["add"],
    do: glint.command(fn(_) { Nil })
      |> glint.unnamed_args(glint.EqArgs(0))
      |> glint.description(add.description),
  )
  |> glint.add(at: ["add", "esbuild"], do: add.esbuild())
  |> glint.add(at: ["add", "tailwind"], do: add.tailwind())
  |> glint.add(
    at: ["build"],
    do: glint.command(fn(_) { Nil })
      |> glint.unnamed_args(glint.EqArgs(0))
      |> glint.description(add.description),
  )
  |> glint.add(at: ["build", "app"], do: build.app())
  |> glint.add(at: ["build", "component"], do: build.component())
  |> glint.add(at: ["start"], do: start.run())
  |> glint.run(args)
}
