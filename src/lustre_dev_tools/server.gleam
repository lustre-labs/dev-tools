import filepath
import gleam/bool
import gleam/erlang/process
import gleam/http/request.{type Request, Request}
import gleam/http/response.{type Response}
import gleam/regex
import gleam/result
import gleam/string_builder
import lustre_dev_tools/cmd
import lustre_dev_tools/error.{type Error, CannotStartDevServer}
import lustre_dev_tools/project
import lustre_dev_tools/server/live_reload
import mist
import simplifile
import wisp

pub fn start(port: Int) -> Result(Nil, Error) {
  let assert Ok(cwd) = cmd.cwd()
  let assert Ok(root) = filepath.expand(filepath.join(cwd, project.root()))

  use make_socket <- result.try(live_reload.start(root))
  use _ <- result.try(
    fn(req: Request(mist.Connection)) -> Response(mist.ResponseData) {
      case request.path_segments(req) {
        // We're going to inject a script that connects to /lustre-dev-tools over
        // websockets. Whenever we detect a file change we can broadcast a reload
        // message and get the client to hard refresh the page.
        ["lustre-dev-tools"] -> make_socket(req)
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

  handler(Request(..req, path: "/index.html"), root)
}

fn inject_live_reload(
  req: wisp.Request,
  root: String,
  k: fn() -> wisp.Response,
) -> wisp.Response {
  let assert Ok(is_interesting) = regex.from_string(".*\\.html$")
  use <- bool.lazy_guard(!regex.check(is_interesting, req.path), k)
  let path = filepath.join(root, req.path)

  case simplifile.verify_is_file(path) {
    Ok(False) | Error(_) -> k()
    Ok(True) -> {
      let assert Ok(html) = simplifile.read(path)

      html
      |> live_reload.inject
      |> string_builder.from_string
      |> wisp.html_response(200)
    }
  }
}
