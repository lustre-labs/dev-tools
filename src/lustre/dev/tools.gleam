import lustre_dev_tools/cli/start
import glint
import gleam/option.{type Option, Some, None}
import gleam/int.{to_string}

pub opaque type Config {
  Config(
    port: Option(Int),
    build_hook: Option(fn()->Nil),
  )
}

pub fn new_config() -> Config {
  Config(None, None)
}

pub fn with_port(cfg: Config, port: Int) -> Config {
  Config(..cfg, port: Some(port))
}

pub fn with_build_hook(cfg: Config, build_hook: fn()->Nil) -> Config {
  Config(..cfg, build_hook: Some(build_hook))
}

pub fn run(cfg: Config) -> Nil {
  let options = {
    case cfg.port {
      Some(port) -> ["--port", to_string(port)]
      None -> []
    }
  }
  let build_hook = case cfg.build_hook {
    Some(f) -> f
    None -> fn(){Nil}
  }
  glint.new()
  |> glint.add(at: [], do: start.run(build_hook))
  |> glint.run(options)
}
