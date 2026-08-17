// IMPORTS ---------------------------------------------------------------------

import collie
import filepath
import gleam/bytes_tree
import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/http
import gleam/http/request.{Request}
import gleam/http/response
import gleam/httpc
import gleam/list
import gleam/option.{Some}
import gleam/otp/actor
import gleam/result
import gleam/string
import gleam/uri.{type Uri}
import lustre_dev_tools/error.{type Error}
import mist
import tom.{type Toml}
import wisp.{type Request, type Response}

// TYPES -----------------------------------------------------------------------

pub type Proxy {
  Proxy(from: String, to: Uri)
}

type WebSocketProxyFromMessage {
  WebSocketProxyFromText(String)
  WebSocketProxyFromBinary(BitArray)
  WebSocketProxyFromClosed
}

type WebSocketProxyToMessage {
  WebSocketProxyToText(String)
  WebSocketProxyToBinary(BitArray)
  WebSocketProxyToClosed
  WebSocketProxyToRespondSubject(process.Subject(WebSocketProxyFromMessage))
}

type WebSocketProxyToRelayState {
  WebSocketProxyToRelayBuffering(List(WebSocketProxyFromMessage))
  WebSocketProxyToRelayForwarding(process.Subject(WebSocketProxyFromMessage))
}

// CONSTRUCTORS ----------------------------------------------------------------

pub fn new(from: String, to: String) -> Result(Proxy, Error) {
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

fn parse_proxy(options: Dict(String, Toml)) -> Result(Proxy, Error) {
  use from <- result.try(
    tom.get_string(options, ["from"])
    |> result.replace_error(error.ProxyMissingFrom),
  )
  use to <- result.try(
    tom.get_string(options, ["to"])
    |> result.replace_error(error.ProxyMissingTo),
  )
  new(from, to)
}

pub fn get_proxies_from_config(
  config: Dict(String, Toml),
  path: List(String),
) -> Result(List(Proxy), Error) {
  case tom.get(config, path) {
    Ok(proxy_toml) -> {
      case proxy_toml {
        tom.InlineTable(table) | tom.Table(table) ->
          table
          |> parse_proxy
          |> result.map(list.wrap)
        tom.Array(array) -> {
          array
          |> list.map(fn(table) {
            case table {
              tom.InlineTable(proxy) | tom.Table(proxy) -> parse_proxy(proxy)
              _ -> Error(error.ProxyInvalidConfig)
            }
          })
          |> result.all
        }
        _ -> Error(error.ProxyInvalidConfig)
      }
    }
    Error(e) -> {
      case e {
        tom.NotFound(_) -> Ok([])
        tom.WrongType(_, _, _) -> Error(error.ProxyInvalidConfig)
      }
    }
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

fn forward_to_proxy_from(
  state: WebSocketProxyToRelayState,
  message: WebSocketProxyFromMessage,
) -> collie.Next(WebSocketProxyToRelayState, WebSocketProxyToMessage) {
  case state {
    WebSocketProxyToRelayForwarding(proxy_from_subject) -> {
      process.send(proxy_from_subject, message)
      collie.continue(state)
    }
    WebSocketProxyToRelayBuffering(pending_proxy_from_messages) ->
      collie.continue(
        WebSocketProxyToRelayBuffering([message, ..pending_proxy_from_messages]),
      )
  }
}

fn is_websocket_upgrade(request: request.Request(mist.Connection)) -> Bool {
  let upgrade_is_websocket = case request.get_header(request, "Upgrade") {
    Ok(value) -> string.lowercase(value) == "websocket"
    Error(_) -> False
  }

  let connection_has_upgrade = case request.get_header(request, "Connection") {
    Ok(value) ->
      value
      |> string.split(on: ",")
      |> list.any(fn(token) {
        string.lowercase(string.trim(token)) == "upgrade"
      })
    Error(_) -> False
  }

  upgrade_is_websocket && connection_has_upgrade
}

pub fn handle_websocket(
  request: request.Request(mist.Connection),
  proxies: List(Proxy),
  next: fn() -> response.Response(mist.ResponseData),
) -> response.Response(mist.ResponseData) {
  case is_websocket_upgrade(request) {
    True -> {
      let response_result = {
        use Proxy(from:, to:) <- list.find_map(proxies)
        case string.split_once(request.path, on: from) {
          Ok(#("", path)) -> {
            let path = filepath.join(to.path, path)
            let assert Some(host) = to.host
            let assert Ok(body) = mist.read_body(request, 8_000_000)

            let proxy_to_connection =
              collie.new(
                Request(
                  ..request,
                  scheme: case option.map(to.scheme, string.lowercase) {
                    Some("https") -> http.Https
                    Some("wss") -> http.Https
                    _ -> http.Http
                  },
                  host:,
                  port: to.port,
                  path:,
                  body:,
                ),
                WebSocketProxyToRelayBuffering([]),
              )
              |> collie.on_message(fn(proxy_to_conn, state, message) {
                case message {
                  collie.Text(text) ->
                    forward_to_proxy_from(state, WebSocketProxyFromText(text))
                  collie.Binary(data) ->
                    forward_to_proxy_from(state, WebSocketProxyFromBinary(data))
                  collie.User(WebSocketProxyToRespondSubject(proxy_from_subject)) -> {
                    case state {
                      WebSocketProxyToRelayBuffering(
                        pending_proxy_from_messages,
                      ) ->
                        pending_proxy_from_messages
                        |> list.reverse
                        |> list.each(process.send(proxy_from_subject, _))
                      WebSocketProxyToRelayForwarding(_) -> Nil
                    }

                    collie.continue(WebSocketProxyToRelayForwarding(
                      proxy_from_subject,
                    ))
                  }
                  collie.User(WebSocketProxyToText(text)) -> {
                    let _ = collie.send_text_frame(proxy_to_conn, text)
                    collie.continue(state)
                  }
                  collie.User(WebSocketProxyToBinary(data)) -> {
                    let _ = collie.send_binary_frame(proxy_to_conn, data)
                    collie.continue(state)
                  }
                  collie.User(WebSocketProxyToClosed) ->
                    collie.send_close_frame(
                      proxy_to_conn,
                      collie.NormalClosure(<<>>),
                    )
                }
              })
              |> collie.on_close(fn(state, _reason) {
                case state {
                  WebSocketProxyToRelayForwarding(proxy_from_subject) ->
                    process.send(proxy_from_subject, WebSocketProxyFromClosed)
                  WebSocketProxyToRelayBuffering(_) -> Nil
                }
              })
              |> collie.start()

            case proxy_to_connection {
              Ok(actor.Started(data: proxy_to_subject, ..)) ->
                Ok(
                  mist.websocket(
                    request: request,
                    on_init: fn(_conn) {
                      let proxy_from_subject = process.new_subject()
                      process.send(
                        proxy_to_subject,
                        collie.to_user_message(WebSocketProxyToRespondSubject(
                          proxy_from_subject,
                        )),
                      )
                      let selector =
                        process.new_selector()
                        |> process.select(proxy_from_subject)
                      #(Nil, Some(selector))
                    },
                    handler: fn(state, message, proxy_from_conn) {
                      case message {
                        mist.Text(text) -> {
                          process.send(
                            proxy_to_subject,
                            collie.to_user_message(WebSocketProxyToText(text)),
                          )
                          mist.continue(state)
                        }
                        mist.Binary(data) -> {
                          process.send(
                            proxy_to_subject,
                            collie.to_user_message(WebSocketProxyToBinary(data)),
                          )
                          mist.continue(state)
                        }
                        mist.Custom(WebSocketProxyFromText(text)) -> {
                          let _ = mist.send_text_frame(proxy_from_conn, text)
                          mist.continue(state)
                        }
                        mist.Custom(WebSocketProxyFromBinary(data)) -> {
                          let _ = mist.send_binary_frame(proxy_from_conn, data)
                          mist.continue(state)
                        }
                        mist.Custom(WebSocketProxyFromClosed) -> mist.stop()
                        mist.Closed | mist.Shutdown -> {
                          process.send(
                            proxy_to_subject,
                            collie.to_user_message(WebSocketProxyToClosed),
                          )
                          mist.stop()
                        }
                      }
                    },
                    on_close: fn(_state) { Nil },
                  ),
                )
              Error(_) -> {
                Ok(next())
              }
            }
          }

          _ -> Error(Nil)
        }
      }
      case response_result {
        Ok(r) -> r
        _ -> next()
      }
    }
    False -> next()
  }
}
