// IMPORTS ---------------------------------------------------------------------

import booklet.{type Booklet}
import filepath
import gleam/erlang/process.{type Pid, type Subject}
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor.{Started}
import gleam/result
import gleam/string
import gleam_community/ansi
import group_registry.{type GroupRegistry}
import justin
import lustre_dev_tools/bin/bun
import lustre_dev_tools/bin/gleam
import lustre_dev_tools/bin/tailwind
import lustre_dev_tools/cli
import lustre_dev_tools/error.{type Error}
import lustre_dev_tools/project.{type Project}
import lustre_dev_tools/system
import polly
import simplifile

// TYPES -----------------------------------------------------------------------

pub type Watcher =
  GroupRegistry(Event)

pub type Event {
  Change(in: String, path: String)
  Styles
  BuildError(reason: Error)
}

//

pub fn start(
  project: Project,
  error: Booklet(Option(Error)),
  watch: List(String),
  tailwind_entry: Option(String),
) -> Result(Watcher, Error) {
  let name = process.new_name("registry")
  let assert Ok(Started(data: registry, ..)) = group_registry.start(name)

  use _ <- result.try(case tailwind_entry {
    Some(entry) ->
      tailwind.watch(
        project,
        entry,
        filepath.join(project.root, "build/dev/javascript"),
        True,
        fn() {
          group_registry.members(registry, "watch")
          |> list.each(process.send(_, Styles))
        },
      )
    None -> Ok(Nil)
  })

  case start_bun_watcher(project, error, watch, registry) {
    Ok(_) -> Ok(registry)
    Error(_) ->
      start_polly_watcher(project, error, watch, registry)
      |> result.replace(registry)
      |> result.replace_error(error.CouldNotStartFileWatcher(
        watcher: "polly",
        os: system.detect_os(),
        arch: system.detect_arch(),
      ))
  }
}

fn start_bun_watcher(
  project: Project,
  error: Booklet(Option(Error)),
  watch: List(String),
  registry: Watcher,
) -> Result(_, _) {
  use dir, path <- bun.watch(project, watch)
  let event = handle_change(project, error, dir, path)

  group_registry.members(registry, "watch")
  |> list.each(process.send(_, event))
}

fn start_polly_watcher(
  project: Project,
  error: Booklet(Option(Error)),
  watch: List(String),
  registry: Watcher,
) -> Result(_, _) {
  let assert [first, ..rest] = watch
  let polly =
    polly.new()
    |> polly.add_dir(first)
    |> polly.ignore_initial_missing
    |> list.fold(rest, _, polly.add_dir)

  use change <- polly.watch(polly)

  case change {
    polly.Changed(path:) | polly.Created(path:) | polly.Deleted(path:) -> {
      let assert Ok(dir) = list.find(watch, string.starts_with(change.path, _))
      let event = handle_change(project, error, dir, path)

      group_registry.members(registry, "watch")
      |> list.each(process.send(_, event))
    }

    polly.Error(..) -> Nil
  }
}

fn handle_change(
  project: Project,
  error: Booklet(Option(Error)),
  dir: String,
  path: String,
) -> Event {
  let result = {
    use _ <- result.try(gleam.build(project))
    use _ <- result.try(case project.has_node_modules {
      True -> {
        let module =
          "import { main } from '../../build/dev/javascript/${name}/${entry}.mjs'; main();"
          |> string.replace("${name}", project.name)
          |> string.replace("${entry}", project.name)

        let name = justin.snake_case(project.name) <> ".dev.mjs"
        let path = filepath.join(project.build, name)

        use _ <- result.try(
          simplifile.write(path, module)
          |> result.map_error(error.CouldNotWriteFile(path, _)),
        )

        bun.build(
          project,
          [path],
          outdir: filepath.join(project.root, "build/dev/javascript"),
          minify: False,
          quiet: True,
        )
      }

      False -> Ok(Nil)
    })

    Ok(Nil)
  }

  case result {
    Ok(_) -> {
      case booklet.get(error) {
        Some(_) -> cli.success("Appliction successfully rebuilt.", False)
        None -> Nil
      }

      booklet.set(error, None)
      Change(in: dir, path:)
    }

    Error(reason) -> {
      booklet.set(error, Some(reason))

      io.println_error(ansi.grey(
        case reason {
          // Compile errors while the dev server is running are going to be quite
          // common so we print the error straight from gleam to cut down on
          // chatty noise.
          error.ExternalCommandFailed(command: "gleam", reason:) -> reason
          _ -> error.explain(reason)
        }
        <> "\n",
      ))

      BuildError(reason:)
    }
  }
}

//

pub fn subscribe(registry: Watcher, client: Pid) -> Subject(Event) {
  group_registry.join(registry, "watch", client)
}

pub fn unsubscribe(registry: Watcher, client: Pid) -> Nil {
  group_registry.leave(registry, "watch", [client])
}
