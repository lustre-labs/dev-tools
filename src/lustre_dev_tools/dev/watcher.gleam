// IMPORTS ---------------------------------------------------------------------

import booklet.{type Booklet}
import filepath
import gleam/erlang/process.{type Pid, type Subject}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor.{Started}
import gleam/result
import gleam/string
import group_registry.{type GroupRegistry}
import lustre_dev_tools/bin/bun
import lustre_dev_tools/bin/gleam
import lustre_dev_tools/bin/tailwind
import lustre_dev_tools/cli
import lustre_dev_tools/error
import lustre_dev_tools/project.{type Project}
import polly

// TYPES -----------------------------------------------------------------------

pub type Watcher =
  GroupRegistry(Event)

pub type Event {
  Change(in: String, path: String)
  BuildError(reason: error.Error)
}

//

pub fn start(
  project: Project,
  error: Booklet(Option(error.Error)),
  watch: List(String),
  tailwind_entry: Option(String),
) -> Watcher {
  let name = process.new_name("registry")
  let assert Ok(Started(data: registry, ..)) = group_registry.start(name)

  case start_bun_watcher(project, error, watch, tailwind_entry, registry) {
    Ok(_) -> registry
    Error(_) ->
      case
        start_polly_watcher(project, error, watch, tailwind_entry, registry)
      {
        Ok(_) -> registry
        Error(_) -> {
          cli.log("Failed to start file watcher", False)
          registry
        }
      }
  }
}

fn start_bun_watcher(
  project: Project,
  error: Booklet(Option(error.Error)),
  watch: List(String),
  tailwind_entry: Option(String),
  registry: Watcher,
) -> Result(_, _) {
  use dir, path <- bun.watch(project, watch)
  let result = {
    use _ <- result.try(gleam.build(project))
    use _ <- result.try(case tailwind_entry {
      Some(entry) ->
        tailwind.build(
          project,
          entry,
          filepath.join(project.root, "build/dev/javascript"),
          False,
          quiet: True,
        )
      None -> Ok(Nil)
    })

    Ok(Nil)
  }

  let event = case result {
    Ok(_) -> {
      booklet.set(error, None)
      Change(in: dir, path:)
    }

    Error(reason) -> {
      booklet.set(error, Some(reason))
      BuildError(reason:)
    }
  }

  group_registry.members(registry, "watch")
  |> list.each(process.send(_, event))
}

fn start_polly_watcher(
  project: Project,
  error: Booklet(Option(error.Error)),
  watch: List(String),
  tailwind_entry: Option(String),
  registry: Watcher,
) -> Result(_, _) {
  let assert [first, ..rest] = watch
  let polly =
    polly.new()
    |> polly.add_dir(first)
    |> list.fold(rest, _, polly.add_dir)

  use event <- polly.watch(polly)

  case event {
    polly.Changed(path:) | polly.Created(path:) | polly.Deleted(path:) -> {
      let assert Ok(dir) = list.find(watch, string.starts_with(event.path, _))
      let result = {
        use _ <- result.try(gleam.build(project))
        use _ <- result.try(case tailwind_entry {
          Some(entry) ->
            tailwind.build(
              project,
              entry,
              filepath.join(project.root, "build/dev/javascript"),
              False,
              quiet: True,
            )
          None -> Ok(Nil)
        })

        Ok(Nil)
      }

      let event = case result {
        Ok(_) -> {
          booklet.set(error, None)
          Change(in: dir, path:)
        }

        Error(reason) -> {
          booklet.set(error, Some(reason))
          BuildError(reason:)
        }
      }

      group_registry.members(registry, "watch")
      |> list.each(process.send(_, event))
    }

    polly.Error(..) -> Nil
  }
}

//

pub fn subscribe(registry: Watcher, client: Pid) -> Subject(Event) {
  group_registry.join(registry, "watch", client)
}

pub fn unsubscribe(registry: Watcher, client: Pid) -> Nil {
  group_registry.leave(registry, "watch", [client])
}
