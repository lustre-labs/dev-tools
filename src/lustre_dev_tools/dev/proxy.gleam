// IMPORTS ---------------------------------------------------------------------

import collie
import filepath
import gleam/bool
import gleam/bytes_tree
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/http
import gleam/http/request.{type Request, Request}
import gleam/http/response.{type Response}
import gleam/httpc
import gleam/list
import gleam/option.{None, Some}
import gleam/otp/actor.{type Started, Started}
import gleam/result
import gleam/string
import gleam/uri.{type Uri, Uri}
import lustre_dev_tools/error.{type Error}
import mist.{type Connection as MistConnection, type ResponseData as MistBody}
import tom.{type Toml}
import wisp.{type Body as WispBody, type Connection as WispConnection}

// TYPES -----------------------------------------------------------------------

pub type Proxy {
  Proxy(from: String, to: Uri)
}

type FromClient {
  ClientSentText(String)
  ClientSentBits(BitArray)
  ClientClosed
}

type FromProxy {
  ProxySentText(String)
  ProxySentBits(BitArray)
  ProxyClosed
  ClientSentSubject(Subject(FromClient))
}

type WebSocketProxyToRelayState {
  Buffering(List(FromClient))
  Connected(Subject(FromClient))
}

// CONSTRUCTORS ----------------------------------------------------------------

pub fn new(from: String, to: String) -> Result(Proxy, Error) {
  case from, uri.parse(to) {
    // No proxy config has been provided
    "", Error(_) -> Error(error.ProxyMissingFromTo)
    // The "from" field is missing
    "", _ -> Error(error.ProxyMissingFrom)
    // The "to" field is missing or it's a uri without a hostname
    _, Error(_) | _, Ok(Uri(host: None, ..)) -> Error(error.ProxyMissingTo)
    // A complete valid proxy config has been provided
    "/" <> _, Ok(Uri(host: Some(_), ..) as to) -> Ok(Proxy(from:, to:))
    // The "from" field is missing a trailing `/`
    _, Ok(Uri(host: Some(_), ..) as to) -> Ok(Proxy(from: "/" <> from, to:))
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

fn match_proxy(
  request: request.Request(body),
  proxies: List(Proxy),
) -> Result(Uri, Nil) {
  list.find_map(proxies, fn(proxy) {
    case string.split_once(request.path, on: proxy.from) {
      Ok(#("", path)) ->
        Ok(Uri(..proxy.to, path: filepath.join(proxy.to.path, path)))
      Ok(_) | Error(_) -> Error(Nil)
    }
  })
}

// MIDDLEWARE ------------------------------------------------------------------

pub fn handle(
  request: Request(WispConnection),
  proxies: List(Proxy),
  next: fn() -> Response(WispBody),
) -> Response(WispBody) {
  case match_proxy(request, proxies) {
    Ok(to) -> {
      let assert Some(host) = to.host
      let assert Ok(body) = wisp.read_body_bits(request)

      Request(
        ..request,
        scheme: case option.map(to.scheme, string.lowercase) {
          Some("https") -> http.Https
          _ -> http.Http
        },
        host:,
        port: to.port,
        path: to.path,
        body:,
      )
      |> httpc.send_bits
      |> result.map(response.map(_, bytes_tree.from_bit_array))
      |> result.map(response.map(_, wisp.Bytes))
      |> result.lazy_unwrap(fn() {
        response.new(500)
        |> response.set_body(wisp.Bytes(bytes_tree.new()))
      })
    }

    Error(_) -> next()
  }
}

fn forward_to_client(
  state: WebSocketProxyToRelayState,
  message: FromClient,
) -> collie.Next(WebSocketProxyToRelayState, FromProxy) {
  case state {
    Connected(client) -> {
      process.send(client, message)
      collie.continue(state)
    }

    Buffering(buffer) -> collie.continue(Buffering([message, ..buffer]))
  }
}

fn is_websocket_upgrade(request: request.Request(mist.Connection)) -> Bool {
  let upgrade_is_websocket = case request.get_header(request, "upgrade") {
    Ok(value) -> string.lowercase(value) == "websocket"
    Error(_) -> False
  }

  let connection_has_upgrade = case request.get_header(request, "connection") {
    Ok(value) ->
      list.any(string.split(value, on: ","), fn(token) {
        string.lowercase(string.trim(token)) == "upgrade"
      })
    Error(_) -> False
  }

  upgrade_is_websocket && connection_has_upgrade
}

pub fn handle_websocket(
  request: Request(MistConnection),
  proxies: List(Proxy),
  next: fn() -> Response(MistBody),
) -> Response(MistBody) {
  use <- bool.lazy_guard(!is_websocket_upgrade(request), next)
  let result = {
    use to <- result.try(match_proxy(request, proxies))
    use Started(data: proxy, ..) <- result.try(start_websocket_proxy(
      request,
      to,
    ))

    Ok(proxy)
  }

  case result {
    Ok(proxy) -> return_websocket_proxy(request, proxy)
    _ -> next()
  }
}

fn start_websocket_proxy(
  request: Request(MistConnection),
  to: Uri,
) -> Result(Started(Subject(collie.WebsocketMessage(FromProxy))), Nil) {
  let assert Some(host) = to.host

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
      path: to.path,
      body: Nil,
    ),
    Buffering([]),
  )
  |> collie.on_message(fn(connection, state, message) {
    case message {
      collie.Text(text) -> forward_to_client(state, ClientSentText(text))
      collie.Binary(data) -> forward_to_client(state, ClientSentBits(data))
      collie.User(ClientSentSubject(client)) -> {
        case state {
          Buffering(buffer) ->
            list.fold_right(buffer, Nil, fn(_, message) {
              process.send(client, message)
            })

          Connected(_) -> Nil
        }

        collie.continue(Connected(client))
      }

      collie.User(ProxySentText(text)) -> {
        let _ = collie.send_text_frame(connection, text)
        collie.continue(state)
      }

      collie.User(ProxySentBits(data)) -> {
        let _ = collie.send_binary_frame(connection, data)
        collie.continue(state)
      }

      collie.User(ProxyClosed) -> {
        let _ = collie.send_close_frame(connection, collie.NoCloseReason)
        collie.continue(state)
      }
    }
  })
  |> collie.on_close(fn(state, _reason) {
    case state {
      Connected(client) -> process.send(client, ClientClosed)
      Buffering(_) -> Nil
    }
  })
  |> collie.start()
  |> result.replace_error(Nil)
}

fn return_websocket_proxy(
  request: Request(MistConnection),
  proxy: Subject(collie.WebsocketMessage(FromProxy)),
) {
  use state, message, connection <- mist.websocket(
    request:,
    on_init: fn(_) {
      let client = process.new_subject()
      let selector = process.new_selector() |> process.select(client)

      process.send(proxy, collie.to_user_message(ClientSentSubject(client)))

      #(Nil, Some(selector))
    },
    on_close: fn(_) { Nil },
  )

  case message {
    mist.Text(text) -> {
      process.send(proxy, collie.to_user_message(ProxySentText(text)))
      mist.continue(state)
    }

    mist.Binary(data) -> {
      process.send(proxy, collie.to_user_message(ProxySentBits(data)))
      mist.continue(state)
    }

    mist.Custom(ClientSentText(text)) -> {
      let _ = mist.send_text_frame(connection, text)
      mist.continue(state)
    }

    mist.Custom(ClientSentBits(data)) -> {
      let _ = mist.send_binary_frame(connection, data)
      mist.continue(state)
    }

    mist.Custom(ClientClosed) -> mist.stop()

    mist.Closed | mist.Shutdown -> {
      process.send(proxy, collie.to_user_message(ProxyClosed))
      mist.stop()
    }
  }
}
