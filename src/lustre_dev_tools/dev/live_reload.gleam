// IMPORTS ---------------------------------------------------------------------

import booklet.{type Booklet}
import filepath
import gleam/erlang/process
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/json
import gleam/option.{type Option, None, Some}
import lustre_dev_tools/dev/watcher.{type Watcher}
import lustre_dev_tools/error
import lustre_dev_tools/project.{type Project}
import mist.{type Connection, type ResponseData}

//

pub fn start(
  request: Request(Connection),
  project: Project,
  error: Booklet(Option(error.Error)),
  watcher: Watcher,
) -> Response(ResponseData) {
  use _, message, connection <- mist.websocket(
    request,
    on_init: fn(connection) {
      let self = process.self()
      let subject = watcher.subscribe(watcher, self)
      let selector = process.new_selector() |> process.select(subject)

      case booklet.get(error) {
        Some(reason) -> {
          let message = error.explain(reason)
          let _ =
            json.object([
              #("type", json.string("error")),
              #("message", json.string(message)),
            ])
            |> json.to_string
            |> mist.send_text_frame(connection, _)

          Nil
        }

        None -> Nil
      }

      #(Nil, Some(selector))
    },
    on_close: fn(_) {
      let self = process.self()

      watcher.unsubscribe(watcher, self)
    },
  )

  case message {
    mist.Custom(watcher.Change(in: dir, path:)) if dir == project.assets -> {
      let _ =
        json.object([
          #("type", json.string("asset-update")),
          #("asset", json.string(path)),
        ])
        |> json.to_string
        |> mist.send_text_frame(connection, _)

      mist.continue(Nil)
    }

    mist.Custom(watcher.Change(path:, ..)) ->
      case filepath.extension(path) {
        Ok("css") -> {
          let _ =
            json.object([
              #("type", json.string("asset-update")),
              #("asset", json.string(path)),
            ])
            |> json.to_string
            |> mist.send_text_frame(connection, _)

          mist.continue(Nil)
        }

        _ -> {
          let _ =
            json.object([
              #("type", json.string("reload")),
            ])
            |> json.to_string
            |> mist.send_text_frame(connection, _)

          mist.continue(Nil)
        }
      }

    mist.Custom(watcher.Styles) -> mist.continue(Nil)

    mist.Custom(watcher.BuildError(reason:)) -> {
      let message = error.explain(reason)

      let _ =
        json.object([
          #("type", json.string("error")),
          #("message", json.string(message)),
        ])
        |> json.to_string
        |> mist.send_text_frame(connection, _)

      mist.continue(Nil)
    }

    mist.Binary(_) | mist.Text(_) -> mist.continue(Nil)

    mist.Closed | mist.Shutdown -> mist.stop()
  }
}
