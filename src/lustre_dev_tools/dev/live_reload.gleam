// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/erlang/process
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/json
import gleam/option
import lustre_dev_tools/bin/gleam
import lustre_dev_tools/dev/watcher.{type Watcher}
import lustre_dev_tools/project.{type Project}
import mist.{type Connection, type ResponseData}

//

pub fn start(
  request: Request(Connection),
  project: Project,
  watcher: Watcher,
) -> Response(ResponseData) {
  use watcher, message, connection <- mist.websocket(
    request,
    on_init: fn(_) {
      let self = process.self()
      let subject = watcher.subscribe(watcher, self)
      let selector = process.new_selector() |> process.select(subject)

      #(Nil, option.Some(selector))
    },
    on_close: fn(_) {
      let self = process.self()

      watcher.unsubscribe(watcher, self)
    },
  )

  case message {
    mist.Custom(#(dir, path)) if dir == project.assets -> {
      let _ =
        json.object([
          #("type", json.string("asset-update")),
          #("asset", json.string(path)),
        ])
        |> json.to_string
        |> mist.send_text_frame(connection, _)

      mist.continue(watcher)
    }

    mist.Custom(#(_, path)) ->
      case filepath.extension(path) {
        Ok("css") -> {
          let _ =
            json.object([
              #("type", json.string("asset-update")),
              #("asset", json.string(path)),
            ])
            |> json.to_string
            |> mist.send_text_frame(connection, _)

          mist.continue(watcher)
        }

        _ -> {
          let _ = gleam.build(project)
          let _ =
            json.object([
              #("type", json.string("reload")),
            ])
            |> json.to_string
            |> mist.send_text_frame(connection, _)

          mist.continue(watcher)
        }
      }

    mist.Binary(_) | mist.Text(_) -> mist.continue(watcher)

    mist.Closed | mist.Shutdown -> mist.stop()
  }
}
