// IMPORTS ---------------------------------------------------------------------

import filepath
import filespy.{type Change}
import gleam/bool
import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import gleam/regex
import gleam/result
import gleam/set
import lustre_dev_tools/cli
import lustre_dev_tools/error.{type Error, CannotStartFileWatcher}
import lustre_dev_tools/cli/build

// TYPES -----------------------------------------------------------------------

pub type Action {
  Add(Subject(Reload))
  Remove(Subject(Reload))
  Broadcast
}

pub type Reload {
  Reload
}

//

pub fn start(root: String) -> Result(Subject(Action), Error) {
  use coordinator <- result.try(start_coordinator())
  use _ <- result.try(start_watcher(coordinator, root))

  Ok(coordinator)
}

fn start_coordinator() -> Result(Subject(Action), Error) {
  let loop = fn(action, clients) {
    case action {
      Add(client) -> actor.continue(set.insert(clients, client))
      Remove(client) -> actor.continue(set.delete(clients, client))
      Broadcast -> {
        set.fold(clients, Nil, fn(_, client) { process.send(client, Reload) })
        actor.continue(clients)
      }
    }
  }

  actor.start(set.new(), loop)
  |> result.map_error(CannotStartFileWatcher)
}

fn start_watcher(
  coordinator: Subject(Action),
  root: String,
) -> Result(Subject(Change), Error) {
  let src = filepath.join(root, "src")
  let assert Ok(is_interesting) = regex.from_string(".*\\.(gleam|m?js)$")

  filespy.new()
  |> filespy.add_dir(src)
  |> filespy.set_handler(fn(path, event) {
    use <- bool.guard(!regex.check(is_interesting, path), Nil)

    case event {
      filespy.Created | filespy.Modified | filespy.Deleted -> {
        let script = build.do_app(False, suppress: True, skip_validation: True)
        case cli.run(script, Nil) {
          Ok(_) -> process.send(coordinator, Broadcast)
          Error(_) -> Nil
        }
      }
      _ -> Nil
    }
  })
  |> filespy.start
  |> result.map_error(CannotStartFileWatcher)
}
