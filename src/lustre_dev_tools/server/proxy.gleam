import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/uri.{type Uri}
import glint
import glint/flag.{type Flag}
import lustre_dev_tools/cli.{type Cli, do, try}
import lustre_dev_tools/project.{type Config}
import tom

// TYPES -----------------------------------------------------------------------

pub type Proxy {
  Proxy(from: String, to: Uri)
}

//

pub fn get_proxy() -> Cli(Option(Proxy)) {
  use from <- do(get_proxy_from())
  use to <- do(get_proxy_to())

  todo
}

fn get_proxy_from() -> Cli(Option(String)) {
  use flags <- do(cli.get_flags())
  use config <- do(cli.get_config())

  let from = result.nil_error(flag.get_string(flags, "proxy-from"))
  let toml =
    result.nil_error(
      tom.get_string(config.toml, ["lustre-dev", "start", "proxy", "from"]),
    )

  result.or(from, toml)
  |> option.from_result
  |> cli.return
}

fn get_proxy_to() -> Cli(Option(String)) {
  use flags <- do(cli.get_flags())
  use config <- do(cli.get_config())

  let to = result.nil_error(flag.get_string(flags, "proxy-to"))
  let toml =
    result.nil_error(
      tom.get_string(config.toml, ["lustre-dev", "start", "proxy", "to"]),
    )

  result.or(to, toml)
  |> option.from_result
  |> cli.return
}
