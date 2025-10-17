////
////

// IMPORTS ---------------------------------------------------------------------

import booklet.{type Booklet}
import filepath
import gleam/erlang/application
import gleam/http
import gleam/http/request
import gleam/int
import gleam/option.{type Option}
import gleam/otp/actor.{type Started}
import gleam/otp/static_supervisor.{type Supervisor}
import gleam/result
import lustre_dev_tools/build/html
import lustre_dev_tools/cli
import lustre_dev_tools/dev/live_reload
import lustre_dev_tools/dev/proxy.{type Proxy}
import lustre_dev_tools/dev/watcher.{type Watcher}
import lustre_dev_tools/error.{type Error}
import lustre_dev_tools/project.{type Project}
import mist
import wisp.{type Request, type Response}
import wisp/wisp_mist

//

type Context {
  Context(
    project: Project,
    entry: String,
    tailwind_entry: Option(String),
    priv: String,
    proxy: Proxy,
  )
}

//

///
///
pub fn start(
  project: Project,
  error: Booklet(Option(Error)),
  watcher: Watcher,
  proxy: Proxy,
  entry: String,
  tailwind_entry: Option(String),
  host: String,
  port: Int,
) -> Result(Started(Supervisor), Error) {
  let assert Ok(priv) = application.priv_directory("lustre_dev_tools")
  let context = Context(project:, entry:, tailwind_entry:, priv:, proxy:)
  let handler = fn(request) {
    case request.path_segments(request) {
      [".lustre", "ws"] ->
        live_reload.start(request, project, error, watcher, tailwind_entry)
      _ -> wisp_mist.handler(handle_wisp_request(_, context), "")(request)
    }
  }

  mist.new(handler)
  |> mist.port(port)
  |> mist.bind(host)
  |> mist.after_start(fn(_, _, _) {
    cli.success(
      "Server started on http://" <> host <> ":" <> int.to_string(port),
      False,
    )
  })
  |> mist.start
  |> result.map_error(error.CouldNotStartDevServer)
}

///
///
fn handle_wisp_request(request: Request, context: Context) -> Response {
  use <- wisp.rescue_crashes
  use request <- wisp.handle_head(request)
  use request <- wisp.csrf_known_header_protection(request)

  use <- wisp.serve_static(request, under: "/.lustre", from: context.priv)

  use <- wisp.serve_static(
    request,
    under: "/",
    from: filepath.join(context.project.root, "build/dev/javascript"),
  )

  use <- wisp.serve_static(request, under: "/", from: context.project.assets)

  use <- proxy.handle(request, context.proxy)

  case request.method, filepath.extension(request.path) {
    // If we get this far then we want to operate in a type of "SPA mode" that
    // serves the main HTML file for any unknown route. We need to make sure we
    // don't do this for unknown _assets_ though so we'll only do this for paths
    // that don't have a file extension.
    http.Get, Error(_) ->
      html.dev(context.project, context.entry, context.tailwind_entry)
      |> wisp.html_body(wisp.ok(), _)

    _, _ -> wisp.not_found()
  }
}
