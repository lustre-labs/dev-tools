import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/uri.{type Uri}
import glint/flag.{type Flag}
import lustre_dev_tools/cli.{type Cli, do, try}
import lustre_dev_tools/project.{type Config}
import tom

// TYPES -----------------------------------------------------------------------

type Proxy {
  Proxy(from: String, to: Uri)
}

//

pub fn get_proxy() -> Cli(Option(Proxy)) {
  use from <- do(get_proxy_from())
  use to <- do(get_proxy_to())

  case from, to {
    Some(from), Some(to) -> Some(Proxy(from, to))
    _, _ -> None
  }
}

fn get_proxy_from() -> Cli(Option(String)) {
  use flags <- do(cli.get_flags())
  use config <- do(cli.get_config())

  result.or(
    glint.get_string(flags, "proxy-from"),
    tom.get_string(config.toml, ["lustre-dev", "start", "proxy", "from"]),
  )
  |> result.to_option
}

fn get_proxy_to() -> Cli(Option(String)) {
  use flags <- do(cli.get_flags())
  use config <- do(cli.get_config())

  result.or(
    glint.get_string(flags, "proxy-to"),
    tom.get_string(config.toml, ["lustre-dev", "start", "proxy", "to"]),
  )
  |> result.to_option
}
