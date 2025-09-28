// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/erlang/process.{type Pid, type Subject}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor.{Started}
import gleam/string
import group_registry.{type GroupRegistry}
import lustre_dev_tools/bin/bun
import lustre_dev_tools/bin/gleam
import lustre_dev_tools/bin/tailwind
import lustre_dev_tools/cli
import lustre_dev_tools/project.{type Project}
import polly

// TYPES -----------------------------------------------------------------------

pub type Watcher =
  GroupRegistry(#(String, String))

//

pub fn start(
  project: Project,
  watch: List(String),
  tailwind_entry: Option(String),
) -> Watcher {
  let name = process.new_name("registry")
  let assert Ok(Started(data: registry, ..)) = group_registry.start(name)

  case start_bun_watcher(project, watch, tailwind_entry, registry) {
    Ok(_) -> registry
    Error(_) ->
      case start_polly_watcher(project, watch, tailwind_entry, registry) {
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
  watch: List(String),
  tailwind_entry: Option(String),
  registry: Watcher,
) -> Result(_, _) {
  use dir, path <- bun.watch(project, watch)
  let _ = gleam.build(project)
  let _ = case tailwind_entry {
    Some(entry) ->
      tailwind.build(
        project,
        entry,
        filepath.join(project.root, "build/dev/javascript"),
        False,
        quiet: True,
      )
    None -> Ok(Nil)
  }

  group_registry.members(registry, "watch")
  |> list.each(process.send(_, #(dir, path)))
}

fn start_polly_watcher(
  project: Project,
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
  let _ = gleam.build(project)
  let _ = case tailwind_entry {
    Some(entry) ->
      tailwind.build(
        project,
        entry,
        filepath.join(project.root, "build/dev/javascript"),
        False,
        quiet: True,
      )
    None -> Ok(Nil)
  }

  let assert Ok(dir) = list.find(watch, string.starts_with(event.path, _))

  case event {
    polly.Changed(path:) | polly.Created(path:) | polly.Deleted(path:) ->
      group_registry.members(registry, "watch")
      |> list.each(process.send(_, #(dir, path)))

    polly.Error(..) -> Nil
  }
}

//

pub fn subscribe(registry: Watcher, client: Pid) -> Subject(#(String, String)) {
  group_registry.join(registry, "watch", client)
}

pub fn unsubscribe(registry: Watcher, client: Pid) -> Nil {
  group_registry.leave(registry, "watch", [client])
}
