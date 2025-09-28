////
////

// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/erlang/application
import gleam/http/request
import gleam/option.{type Option}
import gleam/otp/actor.{type Started}
import gleam/otp/static_supervisor.{type Supervisor}
import gleam/result
import lustre_dev_tools/build/html
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
      [".lustre", "ws"] -> live_reload.start(request, project, watcher)
      _ -> wisp_mist.handler(handle_wisp_request(_, context), "")(request)
    }
  }

  mist.new(handler)
  |> mist.port(port)
  |> mist.bind(host)
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

  let html = html.dev(context.project, context.entry, context.tailwind_entry)

  wisp.html_body(wisp.ok(), html)
}
