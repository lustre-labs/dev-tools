////
////

// IMPORTS ---------------------------------------------------------------------

import booklet.{type Booklet}
import filepath
import gleam/erlang/application
import gleam/erlang/charlist
import gleam/http
import gleam/http/request
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/otp/actor.{type Started}
import gleam/otp/static_supervisor.{type Supervisor}
import gleam/result
import gleam/string
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
) -> Result(Started(Supervisor), Error) {
  let assert Ok(priv) = application.priv_directory("lustre_dev_tools")
  let context = Context(project:, entry:, tailwind_entry:, priv:, proxies:)
  let handler = fn(request) {
    case request.path_segments(request) {
      [".lustre", "ws"] -> live_reload.start(request, project, error, watcher)
      _ -> wisp_mist.handler(handle_wisp_request(_, context), "")(request)
    }
  }
  let assert Ok(interfaces) = network_interfaces()
  let print_message = case host {
    "0.0.0.0" -> {
      let message =
        "Server started on local loopback interface\n"
        <> "   http://"
        <> "127.0.0.1"
        <> ":"
        <> int.to_string(port)
      message |> cli.success(False)

      let message =
        "Server also started on all interfaces\n"
        <> list.map(interfaces, fn(interface) {
          let #(name, address) = interface
          let host =
            [address.0, address.1, address.2, address.3]
            |> list.map(int.to_string)
            |> string.join(".")

          "   http://"
          <> host
          <> ":"
          <> int.to_string(port)
          <> "\t"
          <> charlist.to_string(name)
        })
        |> string.join("\n")

      message |> cli.info(False)
    }
    _ -> {
      let message =
        "Server started on http://" <> host <> ":" <> int.to_string(port)
      message |> cli.success(False)
    }
  }

  mist.new(handler)
  |> mist.port(port)
  |> mist.bind(host)
  |> mist.after_start(fn(_, _, _) { print_message })
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

  use <- proxy.handle(request, context.proxies)

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

pub type InterfaceName =
  charlist.Charlist

pub type InterfaceAddress =
  #(Int, Int, Int, Int)

@external(erlang, "server_ffi", "network_interfaces")
fn network_interfaces() -> Result(List(#(InterfaceName, InterfaceAddress)), Nil)
