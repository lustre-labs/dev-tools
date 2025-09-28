// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/bytes_tree
import gleam/http/request.{Request}
import gleam/http/response
import gleam/httpc
import gleam/option.{Some}
import gleam/result
import gleam/string
import gleam/uri.{type Uri}
import lustre_dev_tools/error.{type Error}
import wisp.{type Request, type Response}

// TYPES -----------------------------------------------------------------------

pub type Proxy {
  Proxy(from: String, to: Uri)
  None
}

// CONSTRUCTORS ----------------------------------------------------------------

pub fn new(from: String, to: String) -> Result(Proxy, Error) {
  case from, to {
    "", "" -> Ok(None)
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

// MIDDLEWARE ------------------------------------------------------------------

pub fn handle(
  request: Request,
  proxy: Proxy,
  next: fn() -> Response,
) -> Response {
  case proxy {
    None -> next()
    Proxy(from:, to:) ->
      case string.split_once(request.path, on: from) {
        Ok(#("", path)) -> {
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
        }

        _ -> next()
      }
  }
}
