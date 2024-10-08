// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/bool
import gleam/erlang/process
import gleam/http/request.{type Request, Request}
import gleam/http/response.{type Response}
import gleam/io
import gleam/option.{None, Some}
import gleam/regex
import gleam/result
import gleam/string_builder
import lustre_dev_tools/cli.{type Cli, do, try}
import lustre_dev_tools/cmd
import lustre_dev_tools/error.{type Error, CannotStartDevServer}
import lustre_dev_tools/project
import lustre_dev_tools/server/live_reload
import lustre_dev_tools/server/proxy
import mist
import simplifile
import wisp
import wisp/wisp_mist

pub fn start(entry: String, port: Int) -> Cli(Nil) {
  let assert Ok(cwd) = cmd.cwd()
  let assert Ok(root) = filepath.expand(filepath.join(cwd, project.root()))

  use proxy <- do(proxy.get())

  case proxy {
    Some(_) ->
      io.println(
        "
[WARNING] Support for proxying requests to another server is currently still
**experimental**. It's functionality or api may change is breaking ways even
between minor versions. If you run into any problems please open an issue over
at https://github.com/lustre-labs/dev-tools/issues/new
      ",
      )
    None -> Nil
  }
  use flags <- do(cli.get_flags())

  use make_socket <- try(live_reload.start(entry, root, flags))
  use _ <- try(
    fn(req: Request(mist.Connection)) -> Response(mist.ResponseData) {
      use <- proxy.middleware(req, proxy)

      case request.path_segments(req) {
        // We're going to inject a script that connects to /lustre-dev-tools over
        // websockets. Whenever we detect a file change we can broadcast a reload
        // message and get the client to hard refresh the page.
        ["lustre-dev-tools"] -> make_socket(req)
        [] ->
          Request(..req, path: "/index.html")
          |> wisp_mist.handler(handler(_, root), "")

        // For everything else we're just going to serve any static files directly
        // from the project's root.
        _ -> wisp_mist.handler(handler(_, root), "")(req)
      }
    }
    |> mist.new
    |> mist.port(port)
    |> mist.start_http
    |> result.map_error(CannotStartDevServer),
  )

  cli.return(process.sleep_forever())
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

  case simplifile.is_file(path) {
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
