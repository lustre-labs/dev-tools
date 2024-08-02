// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/dynamic.{type DecodeError, type Dynamic}
import gleam/erlang
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid, type Selector, type Subject}
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option}
import gleam/otp/actor
import gleam/result
import gleam/set.{type Set}
import gleam/string
import gleam_community/ansi
import glint
import lustre_dev_tools/cli
import lustre_dev_tools/cli/build
import lustre_dev_tools/cli/flag
import lustre_dev_tools/error.{type Error, CannotStartFileWatcher}
import mist
import simplifile

// TYPES -----------------------------------------------------------------------

type WatcherState =
  Set(Subject(SocketMsg))

pub type WatcherMsg {
  Add(Subject(SocketMsg))
  Remove(Subject(SocketMsg))
  Broadcast
  Unknown(Dynamic)
}

type SocketState =
  #(Subject(SocketMsg), Subject(WatcherMsg))

pub type SocketMsg {
  Reload
  ShowError(Error)
}

type LiveReloadingError {
  NoFileWatcherSupportedForOs
  NoFileWatcherInstalled(watcher: Dynamic)
}

// PUBLIC API ------------------------------------------------------------------

pub fn start(
  root: String,
  flags: glint.Flags,
) -> Result(fn(Request(mist.Connection)) -> Response(mist.ResponseData), Error) {
  use watcher <- result.try(start_watcher(root, flags))
  let make_socket = mist.websocket(
    _,
    loop_socket,
    init_socket(watcher, _),
    close_socket,
  )

  Ok(make_socket)
}

pub fn inject(html: String) -> String {
  let assert Ok(priv) = erlang.priv_directory("lustre_dev_tools")
  let assert Ok(source) = simplifile.read(priv <> "/server/live-reload.js")
  let script = "<script>" <> source <> "</script>"

  html
  |> string.replace("</head>", script <> "</head>")
}

// WEB SOCKET ------------------------------------------------------------------

fn init_socket(
  watcher: Subject(WatcherMsg),
  _connection: mist.WebsocketConnection,
) -> #(SocketState, Option(Selector(SocketMsg))) {
  let self = process.new_subject()
  let selector =
    process.new_selector()
    |> process.selecting(self, fn(msg) { msg })
  let state = #(self, watcher)

  process.send(watcher, Add(self))

  #(state, option.Some(selector))
}

fn loop_socket(
  state: SocketState,
  connection: mist.WebsocketConnection,
  msg: mist.WebsocketMessage(SocketMsg),
) -> actor.Next(SocketMsg, SocketState) {
  case msg {
    mist.Text(_) | mist.Binary(_) -> actor.continue(state)

    mist.Custom(Reload) -> {
      let assert Ok(_) =
        mist.send_text_frame(
          connection,
          json.object([#("$", json.string("reload"))]) |> json.to_string,
        )
      actor.continue(state)
    }

    mist.Custom(ShowError(error)) -> {
      let assert Ok(_) =
        mist.send_text_frame(
          connection,
          json.object([
            #("$", json.string("error")),
            #("error", json.string(error.explain(error))),
          ])
            |> json.to_string,
        )
      actor.continue(state)
    }

    mist.Closed | mist.Shutdown -> {
      process.send(state.1, Remove(state.0))
      actor.Stop(process.Normal)
    }
  }
}

fn close_socket(state: SocketState) -> Nil {
  process.send(state.1, Remove(state.0))
}

// FILE WATCHER ----------------------------------------------------------------

fn start_watcher(
  root: String,
  flags: glint.Flags,
) -> Result(Subject(WatcherMsg), Error) {
  actor.start_spec(
    actor.Spec(fn() { init_watcher(root) }, 1000, fn(msg, state) {
      loop_watcher(msg, state, flags)
    }),
  )
  |> result.map_error(CannotStartFileWatcher)
}

fn init_watcher(root: String) -> actor.InitResult(WatcherState, WatcherMsg) {
  let src = filepath.join(root, "src")
  let id = atom.create_from_string(src)

  case check_live_reloading() {
    Ok(_) -> Nil
    Error(NoFileWatcherSupportedForOs) ->
      "⚠️ There's no live reloading support for your os!"
      |> ansi.yellow
      |> io.println

    Error(NoFileWatcherInstalled(watcher)) ->
      {
        "⚠️ You need to install "
        <> string.inspect(watcher)
        <> " for live reloading to work!"
      }
      |> ansi.yellow
      |> io.println
  }

  case fs_start_link(id, src) {
    Ok(_) -> {
      let self = process.new_subject()
      let selector =
        process.new_selector()
        |> process.selecting(self, fn(msg) { msg })
        |> process.selecting_anything(fn(msg) {
          case change_decoder(msg) {
            Ok(broadcast) -> broadcast
            Error(_) -> Unknown(msg)
          }
        })
      let state = set.new()

      fs_subscribe(id)
      actor.Ready(state, selector)
    }

    Error(err) -> {
      actor.Failed("Failed to start watcher: " <> string.inspect(err))
    }
  }
}

fn loop_watcher(
  msg: WatcherMsg,
  state: WatcherState,
  flags: glint.Flags,
) -> actor.Next(WatcherMsg, WatcherState) {
  case msg {
    Add(client) ->
      client
      |> set.insert(state, _)
      |> actor.continue

    Remove(client) ->
      client
      |> set.delete(state, _)
      |> actor.continue

    Broadcast -> {
      let script = {
        use _ <- cli.do(cli.mute())
        use detect_tailwind <- cli.do(
          cli.get_bool(
            "detect_tailwind",
            True,
            ["build"],
            glint.get_flag(_, flag.detect_tailwind()),
          ),
        )
        use _ <- cli.do(build.do_app(False, detect_tailwind))
        use _ <- cli.do(cli.unmute())

        cli.return(Nil)
      }

      case cli.run(script, flags) {
        Ok(_) -> {
          use _, client <- set.fold(state, Nil)
          process.send(client, Reload)
        }

        Error(error) -> {
          error.explain(error) |> io.println_error

          use _, client <- set.fold(state, Nil)
          process.send(client, ShowError(error))
        }
      }

      actor.continue(state)
    }

    Unknown(_) -> actor.continue(state)
  }
}

fn change_decoder(dyn: Dynamic) -> Result(WatcherMsg, List(DecodeError)) {
  let events_decoder = dynamic.element(1, dynamic.list(dynamic.dynamic))
  use events <- result.try(dynamic.element(2, events_decoder)(dyn))

  case list.any(events, is_interesting_event) {
    True -> Ok(Broadcast)
    False -> Error([])
  }
}

type Event {
  Created
  Modified
  Deleted
}

fn is_interesting_event(event: Dynamic) -> Bool {
  event == dynamic.from(Created)
  || event == dynamic.from(Modified)
  || event == dynamic.from(Deleted)
}

// EXTERNALS -------------------------------------------------------------------

@external(erlang, "lustre_dev_tools_ffi", "fs_start_link")
fn fs_start_link(id: Atom, path: String) -> Result(Pid, Dynamic)

@external(erlang, "lustre_dev_tools_ffi", "check_live_reloading")
fn check_live_reloading() -> Result(Nil, LiveReloadingError)

@external(erlang, "fs", "subscribe")
fn fs_subscribe(id: Atom) -> Atom
