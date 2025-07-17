// IMPORTS ---------------------------------------------------------------------

import argv
import filepath
import gleam/bool
import gleam/io
import gleam/list
import gleam/regexp
import gleam/result
import gleam/string
import glint.{type Command}
import justin
import lustre_dev_tools/bin/bun
import lustre_dev_tools/bin/gleam
import lustre_dev_tools/bin/tailwind
import lustre_dev_tools/cli
import lustre_dev_tools/error.{type Error}
import lustre_dev_tools/project.{type Project}
import simplifile

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let result = {
    use project <- result.try(project.initialise())

    let args = argv.load().arguments
    let cli =
      glint.new()
      |> glint.pretty_help(glint.default_pretty_help())
      |> glint.add(at: ["add"], do: add(project))
      |> glint.add(at: ["build"], do: build(project))
      // |> glint.add(at: ["eject"], do: eject(project))
      // |> glint.add(at: ["gen"], do: todo)
      // |> glint.add(at: ["mcp"], do: todo)
      |> glint.add(at: ["start"], do: start(project))
      |> glint.add(at: [], do: start(project))

    case glint.execute(cli, args) {
      Ok(glint.Help(help)) -> Ok(io.println(help))
      Ok(glint.Out(Ok(_))) -> Ok(Nil)
      Ok(glint.Out(Error(reason))) -> Error(reason)
      Error(message) -> Ok(io.println_error(message))
    }
  }

  case result {
    Ok(_) -> Nil
    Error(reason) -> io.println_error(error.explain(reason))
  }
}

// COMMANDS --------------------------------------------------------------------

type AddOptions {
  AddOptions(integration: String)
}

fn add(project: Project) -> Command(Result(Nil, Error)) {
  use get_integration <- glint.named_arg("integration")
  use args, _, _ <- glint.command
  let options = AddOptions(integration: get_integration(args))

  case options.integration {
    "bun" -> bun.download(project)
    "tailwind" | "tailwindcss" | "tw" -> tailwind.download(project)
    name -> Error(error.UnknownIntegration(name:))
  }
}

type BuildOptions {
  BuildOptions(minify: Bool, outdir: String, entries: List(String))
}

fn build(project: Project) -> Command(Result(Nil, Error)) {
  use <- glint.unnamed_args(glint.MinArgs(0))
  use get_minify <- cli.flag("minify", cli.bool(project), {
    "
Produce a production-ready minified build of the project. This will rename
variables, remove white space, and perform other optimisations to reduce the
size of the JavaScript output.
    "
  })

  use get_outdir <- cli.flag("outdir", cli.string(project), {
    "
    "
  })

  use _, entries, flags <- glint.command
  let options =
    BuildOptions(
      minify: get_minify(flags) |> result.unwrap(False),
      outdir: filepath.join(
        project.root,
        get_outdir(flags) |> result.unwrap("priv/static"),
      ),
      // If the user did not provide any explicit entry modules, we'll take the
      // app's main module as the entry.
      entries: case entries {
        [] -> [project.name]
        _ -> entries
      },
    )

  use _ <- result.try(gleam.build(project))
  use _ <- result.try(
    simplifile.create_directory_all(options.outdir)
    |> result.map_error(error.CouldNotWriteFile(options.outdir, _)),
  )

  use entries <- result.try({
    use entry <- list.try_map(options.entries)
    let module =
      "import { main } from '../../build/dev/javascript/${name}/${entry}.mjs'; main();"
      |> string.replace("${name}", project.name)
      |> string.replace("${entry}", entry)

    let name = justin.snake_case(entry) <> ".mjs"
    let path = filepath.join(project.build, name)

    use _ <- result.try(
      simplifile.write(path, module)
      |> result.map_error(error.CouldNotWriteFile(path, _)),
    )

    Ok(path)
  })

  use _ <- result.try(bun.build(
    project,
    entries,
    options.outdir,
    options.minify,
  ))

  use should_run_tailwind <- result.try(tailwind.detect(project))
  use <- bool.guard(should_run_tailwind != tailwind.HasTailwindEntry, Ok(Nil))
  use _ <- result.try(tailwind.build(
    project,
    filepath.join(project.src, project.name <> ".css"),
    filepath.join(options.outdir, project.name <> ".css"),
    options.minify,
  ))

  Ok(Nil)
}

type EjectOptions {
  EjectOptions(build_tool: String)
}

fn eject(project: Project) -> Command(Result(Nil, Error)) {
  use get_build_tool <- glint.named_arg("build-tool")
  use args, _, _ <- glint.command
  let options = EjectOptions(build_tool: get_build_tool(args))

  // 1. Generate a top-level `index.html` and any other internal files we have
  //    that now need to be exposed

  // 2. Eject the project's internal `package.json`, add `node_modules` to the
  //    user's `.gitignore` if it exists.

  // 3. Remove the internal `.lustre` directory, remove `.lustre` from the user's
  //    `.gitignore` if it exists.

  // 4. Remove `lustre_dev_tools` as a dev dependency from the user's Gleam
  //    project.

  case options.build_tool {
    "bun" -> {
      todo
    }

    "vite" -> {
      // 5. Generate a `vite.config.js` file in the root of the project.

      todo
    }

    name -> Error(error.UnknownBuildTool(name:))
  }
}

fn start(project: Project) -> Command(Result(Nil, Error)) {
  use _, _, _ <- glint.command

  Error(error.Todo)
}
