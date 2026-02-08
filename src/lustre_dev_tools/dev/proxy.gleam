// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/bytes_tree
import gleam/dict.{type Dict}
import gleam/http/request.{Request}
import gleam/http/response
import gleam/httpc
import gleam/list
import gleam/option.{Some}
import gleam/result
import gleam/string
import gleam/uri.{type Uri}
import lustre_dev_tools/error.{type Error}
import tom.{type Toml}
import wisp.{type Request, type Response}

// TYPES -----------------------------------------------------------------------

pub type Proxy {
  Proxy(from: String, to: Uri)
}

// CONSTRUCTORS ----------------------------------------------------------------

pub fn new(from: String, to: String) -> Result(Proxy, Nil) {
  case from, to {
    "", "" -> Error(error.ProxyMissingFromTo)
    "", _ -> Error(error.ProxyMissingFrom)
    _, "" -> Error(error.ProxyMissingTo)
    "/" <> _, _ ->
      case uri.parse(to) {
        Ok(uri) -> Ok(Proxy(from:, to: uri))
        Error(_) -> Error(error.ProxyInvalidTo)
      }
    _, _ ->
      case uri.parse(to) {
        Ok(uri) -> Ok(Proxy(from: "/" <> from, to: uri))
        Error(_) -> Error(error.ProxyInvalidTo)
      }
  }
}

// EXTRACTORS ------------------------------------------------------------------

fn parse_proxy(options: Dict(String, Toml)) -> Result(Proxy, Nil) {
  use from <- result.try(
    tom.get_string(options, ["from"]) |> result.replace_error(Nil),
  )
  use to <- result.try(
    tom.get_string(options, ["to"]) |> result.replace_error(Nil),
  )
  use proxy <- result.try(new(from, to) |> result.replace_error(Nil))
  Ok(proxy)
}

pub fn get_proxies_from_config(
  config: Dict(String, Toml),
  path: List(String),
) -> Result(List(Proxy), Nil) {
  use proxy_toml <- result.try(
    tom.get(config, path) |> result.replace_error(Nil),
  )
  case proxy_toml {
    tom.InlineTable(table) ->
      table
      |> parse_proxy
      |> result.map(list.wrap)
    tom.Array(array) -> {
      array
      |> list.map(fn(table) {
        case table {
          tom.InlineTable(proxy) -> parse_proxy(proxy)
          _ -> Error(Nil)
        }
      })
      |> result.all
    }
    _ -> Error(Nil)
  }
}

// MIDDLEWARE ------------------------------------------------------------------

pub fn handle(
  request: Request,
  proxies: List(Proxy),
  next: fn() -> Response,
) -> Response {
  let response_result = {
    use Proxy(from:, to:) <- list.find_map(proxies)
    case string.split_once(request.path, on: from) {
      Ok(#("", path)) -> {
        echo "matched on " <> path
        let internal_error =
          response.new(500)
          |> response.set_body(wisp.Bytes(bytes_tree.new()))

        let path = filepath.join(to.path, path)
        let assert Some(host) = to.host
        let assert Ok(body) = wisp.read_body_bits(request)

        Request(..request, host:, port: to.port, path:, body:)
        |> httpc.send_bits
        |> result.map(response.map(_, bytes_tree.from_bit_array))
        |> result.map(response.map(_, wisp.Bytes))
        |> result.unwrap(internal_error)
        |> Ok
      }
      _ -> Error(Nil)
    }
  }
  case response_result {
    Ok(r) -> r
    _ -> next()
  }
}
