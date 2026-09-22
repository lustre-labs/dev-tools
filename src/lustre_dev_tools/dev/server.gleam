////
////

// IMPORTS ---------------------------------------------------------------------

import booklet.{type Booklet}
import filepath
import gleam/bool
import gleam/erlang/application
import gleam/erlang/charlist.{type Charlist}
import gleam/function
import gleam/http
import gleam/http/request
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
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
    proxies: List(Proxy),
    timeout: Int,
    max_body_size: Int,
  )
}

//

///
///
pub fn start(
  project: Project,
  error: Booklet(Option(Error)),
  watcher: Watcher,
  proxies: List(Proxy),
  entry: String,
  tailwind_entry: Option(String),
  host: String,
  port: Int,
  tls: Option(#(String, String)),
  timeout: Int,
  max_body_size: Int,
) -> Result(Started(Supervisor), Error) {
  let assert Ok(priv) = application.priv_directory("lustre_dev_tools")
  let context =
    Context(
      project:,
      entry:,
      tailwind_entry:,
      priv:,
      proxies:,
      timeout:,
      max_body_size:,
    )

  let handler = fn(request) {
    case request.path_segments(request) {
      [".lustre", "ws"] -> live_reload.start(request, project, error, watcher)
      _ ->
        proxy.handle_websocket(request, context.proxies, fn() {
          wisp_mist.handler(handle_wisp_request(_, context), "")(request)
        })
    }
  }

  mist.new(handler)
  |> mist.port(port)
  |> mist.bind(host)
  |> case tls {
    Some(#(certfile, keyfile)) -> mist.with_tls(_, certfile:, keyfile:)
    None -> function.identity
  }
  |> mist.after_start(fn(port, scheme, _) {
    print_start_message(host, port, scheme)
  })
  |> mist.start
  |> result.map_error(error.CouldNotStartDevServer)
}

fn print_start_message(host, port, scheme) {
  let port = int.to_string(port)
  let scheme = http.scheme_to_string(scheme)

  use <- bool.lazy_guard(host != "0.0.0.0", fn() {
    cli.success(
      "Server started on " <> scheme <> "://" <> host <> ":" <> port,
      False,
    )
  })

  let interfaces = network_interfaces()
  use <- bool.guard(list.is_empty(interfaces), Nil)

  cli.success("Server started on " <> scheme <> "://127.0.0.1:" <> port, False)

  let message = "Server also accessible on:"
  let message = {
    use message, #(name, ip) <- list.fold(interfaces, message)
    let ip = mist.IpV4(ip.0, ip.1, ip.2, ip.3)
    let host = mist.ip_address_to_string(ip)

    message
    <> "\n     "
    <> scheme
    <> "://"
    <> host
    <> ":"
    <> port
    <> "\t"
    <> charlist.to_string(name)
  }

  cli.info(message, False)
}

///
///
fn handle_wisp_request(request: Request, context: Context) -> Response {
  let request =
    wisp.set_max_body_size(request, context.max_body_size * 1024 * 1024)

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
  use <- proxy.handle(request, context.proxies, context.timeout)

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

@external(erlang, "server_ffi", "network_interfaces")
fn network_interfaces() -> List(#(Charlist, #(Int, Int, Int, Int)))
