import filepath
import gleam/bool
import gleam/bytes_tree
import gleam/http/request.{type Request, Request}
import gleam/http/response.{type Response}
import gleam/httpc
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/uri.{type Uri}
import glint
import lustre_dev_tools/cli.{type Cli, do}
import lustre_dev_tools/cli/flag
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
    Some(Proxy(from, to)) ->
      case string.split_once(req.path, on: from) {
        Ok(#("", path)) -> {
          let internal_error =
            response.new(500)
            |> response.set_body(mist.Bytes(bytes_tree.new()))

          let path = filepath.join(to.path, path)
          let assert Some(host) = to.host
          let assert Ok(req) = mist.read_body(req, 100 * 1024 * 1024)

          Request(..req, host:, port: to.port, path:)
          |> httpc.send_bits
          |> result.map(response.map(_, bytes_tree.from_bit_array))
          |> result.map(response.map(_, mist.Bytes))
          |> result.unwrap(internal_error)
        }

        _ -> k()
      }
  }
}

pub fn get() -> Cli(Option(Proxy)) {
  use from <- do(get_proxy_from())
  use to <- do(get_proxy_to())

  case from, to {
    Some(from), Some(to) -> cli.return(Some(Proxy(from, to)))
    Some(_), None -> cli.throw(IncompleteProxy(["proxy-to"]))
    None, Some(_) -> cli.throw(IncompleteProxy(["proxy-from"]))
    None, None -> cli.return(None)
  }
}

fn get_proxy_from() -> Cli(Option(String)) {
  use flags <- do(cli.get_flags())
  use config <- do(cli.get_config())

  let flag = result.replace_error(glint.get_flag(flags, flag.proxy_from()), Nil)
  let toml =
    result.replace_error(
      tom.get_string(config.toml, ["lustre-dev", "start", "proxy", "from"]),
      Nil,
    )

  result.or(flag, toml)
  |> option.from_result
  |> cli.return
}

fn get_proxy_to() -> Cli(Option(Uri)) {
  use flags <- do(cli.get_flags())
  use config <- do(cli.get_config())

  let flag = result.replace_error(glint.get_flag(flags, flag.proxy_to()), Nil)
  let toml =
    result.replace_error(
      tom.get_string(config.toml, ["lustre-dev", "start", "proxy", "to"]),
      Nil,
    )

  let from = result.or(flag, toml)
  use <- bool.guard(from == Error(Nil), cli.return(None))
  let assert Ok(from) = from

  case uri.parse(from) {
    Ok(from) -> cli.return(Some(from))
    Error(_) -> cli.throw(InvalidProxyTarget(from))
  }
}
