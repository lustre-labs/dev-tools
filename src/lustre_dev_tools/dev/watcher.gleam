// IMPORTS ---------------------------------------------------------------------

import booklet.{type Booklet}
import filepath
import gleam/erlang/process.{type Pid, type Subject, type Timer}
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
  let assert Ok(Started(data: builder, ..)) =
    start_build_actor(project, error, registry)

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

  case start_bun_watcher(project, watch, tailwind_entry, builder) {
    Ok(_) -> Ok(registry)
    Error(_) -> {
      cli.log(
        "Failed to start file watcher, falling back to polling watcher.",
        False,
      )

      start_polly_watcher(watch, tailwind_entry, builder)
      |> result.replace(registry)
      |> result.replace_error(error.CouldNotStartFileWatcher(
        watcher: "polly",
        os: system.detect_os(),
        arch: system.detect_arch(),
      ))
    }
  }
}

fn start_bun_watcher(
  project: Project,
  watch: List(String),
  tailwind_entry: Option(String),
  builder: Builder,
) -> Result(_, _) {
  use dir, path <- bun.watch(project, watch)

  case Some(path) == option.map(tailwind_entry, string.append(_, ".css")) {
    True -> Nil
    False -> process.send(builder, FileChanged(in: dir, path:))
  }
}

fn start_polly_watcher(
  watch: List(String),
  tailwind_entry: Option(String),
  builder: Builder,
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
      let assert Ok(dir) = list.find(watch, string.starts_with(path, _))

      case Some(path) == option.map(tailwind_entry, string.append(_, ".css")) {
        True -> Nil
        False -> process.send(builder, FileChanged(in: dir, path:))
      }
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

// BUILD ACTOR -----------------------------------------------------------------

type Builder =
  Subject(BuildMessage)

type BuildState {
  Waiting(self: Subject(BuildMessage))
  Buffering(self: Subject(BuildMessage), timer: Timer)
  Building(
    self: Subject(BuildMessage),
    in: String,
    path: String,
    queued: Option(#(String, String)),
  )
}

type BuildMessage {
  BuildFinished(result: Result(Nil, Error))
  FileChanged(in: String, path: String)
  TimerCompleted(in: String, path: String)
}

const build_debounce_ms = 50

fn start_build_actor(
  project: Project,
  error: Booklet(Option(Error)),
  watcher: Watcher,
) -> Result(actor.Started(Builder), actor.StartError) {
  actor.new_with_initialiser(1000, fn(self) {
    Waiting(self:)
    |> actor.initialised
    |> actor.returning(self)
    |> Ok
  })
  |> actor.on_message(fn(state, message) {
    case state, message {
      Waiting(..), BuildFinished(..) -> actor.continue(state)

      Waiting(self:), FileChanged(in:, path:) -> {
        let timer =
          process.send_after(
            self,
            build_debounce_ms,
            TimerCompleted(in:, path:),
          )

        actor.continue(Buffering(self:, timer:))
      }

      Waiting(self:), TimerCompleted(in:, path:) -> {
        let _ = build_project(project, self)

        actor.continue(Building(self:, in:, path:, queued: None))
      }

      Buffering(..), BuildFinished(..) -> actor.continue(state)

      Buffering(self:, timer:), FileChanged(in:, path:) -> {
        let _ = process.cancel_timer(timer)
        let timer =
          process.send_after(
            self,
            build_debounce_ms,
            TimerCompleted(in:, path:),
          )

        actor.continue(Buffering(self:, timer:))
      }

      Buffering(self:, ..), TimerCompleted(in:, path:) -> {
        let _ = build_project(project, self)

        actor.continue(Building(self:, in:, path:, queued: None))
      }

      Building(self:, in:, path:, queued:), BuildFinished(Ok(_)) -> {
        booklet.set(error, None)

        group_registry.members(watcher, "watch")
        |> list.each(process.send(_, Change(in:, path:)))

        case queued {
          None -> actor.continue(Waiting(self:))
          Some(queued) -> {
            let _ = build_project(project, self)

            actor.continue(Building(
              self:,
              in: queued.0,
              path: queued.1,
              queued: None,
            ))
          }
        }
      }

      Building(self:, queued: None, ..), BuildFinished(Error(reason)) -> {
        booklet.set(error, Some(reason))

        io.println_error(
          case reason {
            // Compile errors while the dev server is running are going to be quite
            // common so we print the error straight from gleam to cut down on
            // chatty noise.
            error.ExternalCommandFailed(command: "gleam", reason:) -> reason
            _ -> ansi.gray(error.explain(reason))
          }
          <> "\n",
        )

        group_registry.members(watcher, "watch")
        |> list.each(process.send(_, BuildError(reason:)))

        actor.continue(Waiting(self:))
      }

      // We have another build immediately queued up so instead of noisily reporting
      // errors we're going to drop them and run the next build: if the error is
      // still there it will be reported then.
      Building(self:, queued: Some(queued), ..), BuildFinished(Error(_)) -> {
        let _ = build_project(project, self)

        actor.continue(Building(
          self:,
          in: queued.0,
          path: queued.1,
          queued: None,
        ))
      }

      Building(..), FileChanged(in:, path:) ->
        actor.continue(Building(..state, queued: Some(#(in, path))))

      Building(..), TimerCompleted(in:, path:) ->
        actor.continue(Building(..state, queued: Some(#(in, path))))
    }
  })
  |> actor.start
}

fn build_project(project: Project, actor: Subject(BuildMessage)) -> Pid {
  use <- process.spawn
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

  process.send(actor, BuildFinished(result:))
}
