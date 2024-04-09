import filepath
import gleam/bool
import gleam/erlang/process.{type Subject}
import gleam/http/request.{type Request, Request}
import gleam/http/response.{type Response}
import gleam/option.{Some}
import gleam/otp/actor
import gleam/regex
import gleam/result
import gleam/string
import gleam/string_builder
import lustre_dev_tools/cli
import lustre_dev_tools/error.{type Error, CannotStartDevServer}
import lustre_dev_tools/project
import lustre_dev_tools/server/live_reload.{
  type Action, type Reload, Add, Reload, Remove,
}
import mist
import simplifile
import wisp

pub fn start(port: Int) -> Result(Nil, Error) {
  let assert Ok(cwd) = cli.cwd()
  let assert Ok(root) = filepath.expand(filepath.join(cwd, project.root()))

  use live_reload <- result.try(live_reload.start(root))
  use _ <- result.try(
    fn(req: Request(mist.Connection)) -> Response(mist.ResponseData) {
      case request.path_segments(req) {
        // We're going to inject a script that connects to /lustre-dev-tools over
        // websockets. Whenever we detect a file change we can broadcast a reload
        // message and get the client to hard refresh the page.
        ["lustre-dev-tools"] -> init_socket(live_reload, req)
        [] ->
          wisp.mist_handler(handler(_, root), "")(
            Request(..req, path: "/index.html"),
          )
        // For everything else we're just going to serve any static files directly
        // from the project's root.
        _ -> wisp.mist_handler(handler(_, root), "")(req)
      }
    }
    |> mist.new
    |> mist.port(port)
    |> mist.start_http
    |> result.map_error(CannotStartDevServer),
  )

  Ok(process.sleep_forever())
}

fn handler(req: wisp.Request, root: String) -> wisp.Response {
  use <- inject_live_reload(req, root)
  use <- wisp.serve_static(req, under: "/", from: root)

  wisp.not_found()
}

import gleam/io

fn inject_live_reload(
  req: wisp.Request,
  root: String,
  k: fn() -> wisp.Response,
) -> wisp.Response {
  let assert Ok(is_interesting) = regex.from_string(".*\\.html$")
  io.debug(req.path)
  use <- bool.lazy_guard(!regex.check(is_interesting, req.path), k)
  let path = filepath.join(root, req.path)

  io.debug(path)

  case simplifile.verify_is_file(path) {
    Ok(False) | Error(_) -> k()
    Ok(True) -> {
      let assert Ok(html) = simplifile.read(path)
      let script =
        "<script>
          function connect() {
            const socket = new WebSocket(`/lustre-dev-tools`);

            socket.onmessage = (event) => {
              if (event.data === 'reload') {
                window.location.reload();
              }
            };

            // If the dev server goes down we'll continue to try to reconnect
            // every 5 seconds. If the user needs to kill the server for some
            // reason, this means the page will restore live reload without a
            // refresh.
            socket.onclose = () => {
              socket = null;
              setTimeout(() => connect(), 5000);
            };
          }

          connect();
        </script>"

      // Inject the live reload script at the end of the `<head />` tag.
      html
      |> string.replace("</head>", script <> "</head>")
      |> string_builder.from_string
      |> wisp.html_response(200)
    }
  }
}

fn init_socket(
  coordinator: Subject(Action),
  req: Request(mist.Connection),
) -> Response(mist.ResponseData) {
  let init = fn(_) {
    let socket = process.new_subject()
    let selector =
      process.selecting(process.new_selector(), socket, fn(a) { a })

    process.send(coordinator, Add(socket))

    #(socket, Some(selector))
  }

  let close = fn(self) { process.send(coordinator, Remove(self)) }

  use watcher, conn, message <- mist.websocket(req, _, init, close)

  case message {
    mist.Text(_) | mist.Binary(_) -> actor.continue(watcher)

    mist.Custom(Reload) -> {
      let assert Ok(_) = mist.send_text_frame(conn, "reload")
      actor.continue(watcher)
    }

    mist.Closed | mist.Shutdown -> {
      close(watcher)
      actor.Stop(process.Normal)
    }
  }
}
