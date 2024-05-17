import filepath
import gleam/bool
import gleam/bytes_builder
import gleam/http/request.{type Request, Request}
import gleam/http/response.{type Response}
import gleam/httpc
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/uri.{type Uri}
import lustre_dev_tools/cli.{type Cli, do}
import lustre_dev_tools/error.{IncompleteProxy, InvalidProxyTarget}
import mist
import tom

// TYPES -----------------------------------------------------------------------

pub type Proxy {
  Proxy(from: String, to: Uri)
}

//

pub fn middleware(
  req: Request(mist.Connection),
  proxy: Option(Proxy),
  k: fn() -> Response(mist.ResponseData),
) -> Response(mist.ResponseData) {
  case proxy {
    None -> k()
    Some(Proxy(from, to)) -> {
      use <- bool.lazy_guard(!string.starts_with(req.path, from), k)

      let path = string.replace(req.path, from, "")
      let assert Ok(req) = mist.read_body(req, 100 * 1024 * 1024)
      let assert Ok(proxy_req) = request.from_uri(to)

      let server_error =
        response.new(404)
        |> response.set_body(mist.Bytes(bytes_builder.new()))

      proxy_req
      |> request.set_body(req.body)
      |> request.set_path(filepath.join(proxy_req.path, path))
      |> list.fold(
        req.headers,
        _,
        fn(req, header) { request.set_header(req, header.0, header.1) },
      )
      |> httpc.send_bits
      |> result.map(response.map(_, bytes_builder.from_bit_array))
      |> result.map(response.map(_, mist.Bytes))
      |> result.unwrap(server_error)
    }
  }
}

pub fn get(
  from_flag: Result(String, Nil),
  to_flag: Result(String, Nil),
) -> Cli(Option(Proxy)) {
  use from <- do(get_proxy_from(from_flag))
  use to <- do(get_proxy_to(to_flag))

  case from, to {
    Some(from), Some(to) -> cli.return(Some(Proxy(from, to)))
    Some(_), None -> cli.throw(IncompleteProxy(["proxy-to"]))
    None, Some(_) -> cli.throw(IncompleteProxy(["proxy-from"]))
    None, None -> cli.return(None)
  }
}

fn get_proxy_from(flag: Result(String, Nil)) -> Cli(Option(String)) {
  use config <- do(cli.get_config())

  let toml =
    result.nil_error(
      tom.get_string(config.toml, ["lustre-dev", "start", "proxy", "from"]),
    )

  result.or(flag, toml)
  |> option.from_result
  |> cli.return
}

fn get_proxy_to(flag: Result(String, Nil)) -> Cli(Option(Uri)) {
  use config <- do(cli.get_config())

  let toml =
    result.nil_error(
      tom.get_string(config.toml, ["lustre-dev", "start", "proxy", "to"]),
    )

  let from = result.or(flag, toml)
  use <- bool.guard(from == Error(Nil), cli.return(None))
  let assert Ok(from) = from

  case uri.parse(from) {
    Ok(from) -> cli.return(Some(from))
    Error(_) -> cli.throw(InvalidProxyTarget(from))
  }
}
