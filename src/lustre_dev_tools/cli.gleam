//// The `cli` module is how we create "scripts" that are intended to be run from
//// the command line.

// IMPORTS ---------------------------------------------------------------------

import gleam/dict
import gleam/erlang
import gleam/io
import gleam/list
import gleam/result
import gleam_community/ansi
import glint
import lustre_dev_tools/error.{type Error, TemplateMissing}
import lustre_dev_tools/project.{type Config}
import simplifile
import spinner.{type Spinner}
import tom

// TYPES -----------------------------------------------------------------------

///
///
pub opaque type Cli(a) {
  Cli(run: fn(Env) -> #(Env, Result(a, Error)))
}

type Env {
  Env(muted: Bool, spinner: SpinnerStatus, config: Config)
}

type SpinnerStatus {
  Running(spinner: Spinner, message: String)
  Paused
}

// RUNNING CLI SCRIPTS ---------------------------------------------------------

///
///
pub fn run(step: Cli(a)) -> Result(a, Error) {
  use config <- result.try(project.config())
  let env = Env(muted: False, spinner: Paused, config: config)
  let #(env, result) = step.run(env)

  case env.spinner {
    Running(spinner, _) -> spinner.stop(spinner)
    Paused -> Nil
  }

  case result, env.spinner {
    // In case the spinner was still running when we got an error we print
    // the message where the spinner got stuck.
    Error(_), Running(_, message) -> io.println("❌ " <> ansi.red(message))
    Error(_), _ | Ok(_), _ -> Nil
  }

  result
}

// CREATING CLI SCRIPTS FROM SIMPLE VALUES -------------------------------------

///
///
pub fn return(value: a) -> Cli(a) {
  use env <- Cli

  #(env, Ok(value))
}

///
///
pub fn throw(error: Error) -> Cli(a) {
  use env <- Cli

  #(env, Error(error))
}

pub fn from_result(result: Result(a, Error)) -> Cli(a) {
  use env <- Cli

  #(env, result)
}

// COMBINATORS -----------------------------------------------------------------

///
///
pub fn do(step: Cli(a), then next: fn(a) -> Cli(b)) -> Cli(b) {
  use env <- Cli
  let #(env, result) = step.run(env)

  case result {
    Ok(value) -> next(value).run(env)
    Error(error) -> {
      case env.spinner {
        Running(spinner, _message) -> spinner.stop(spinner)
        Paused -> Nil
      }

      #(env, Error(error))
    }
  }
}

pub fn in(value: fn() -> a) -> Cli(a) {
  use env <- Cli

  #(env, Ok(value()))
}

pub fn map(step: Cli(a), then next: fn(a) -> b) -> Cli(b) {
  use env <- Cli
  let #(env, result) = step.run(env)
  let result = result.map(result, next)

  #(env, result)
}

///
///
pub fn try(result: Result(a, Error), then next: fn(a) -> Cli(b)) -> Cli(b) {
  use env <- Cli

  case result {
    Ok(a) -> next(a).run(env)
    Error(error) -> {
      case env.spinner {
        Running(spinner, _message) -> spinner.stop(spinner)
        Paused -> Nil
      }

      #(env, Error(error))
    }
  }
}

// LOGGING ---------------------------------------------------------------------

///
///
pub fn log(message: String, then next: fn() -> Cli(a)) -> Cli(a) {
  use env <- Cli
  let env = case env.muted {
    True -> env
    False ->
      Env(
        ..env,
        spinner: case env.spinner {
          Paused ->
            Running(
              spinner.new(message)
                |> spinner.with_colour(ansi.magenta)
                |> spinner.with_frames(spinner.snake_frames)
                |> spinner.start,
              message,
            )

          Running(spinner, _) -> {
            spinner.set_text(spinner, message)
            Running(spinner, message)
          }
        },
      )
  }

  next().run(env)
}

pub fn success(message: String, then next: fn() -> Cli(a)) -> Cli(a) {
  use env <- Cli
  let env =
    Env(
      ..env,
      spinner: case env.spinner {
        Paused -> Paused
        Running(spinner, _) -> {
          spinner.stop(spinner)
          Paused
        }
      },
    )

  case env.muted {
    True -> Nil
    False -> io.println("✅ " <> ansi.green(message))
  }

  next().run(env)
}

pub fn mute() -> Cli(Nil) {
  use env <- Cli

  #(Env(..env, muted: True), Ok(Nil))
}

pub fn unmute() -> Cli(Nil) {
  use env <- Cli

  #(Env(..env, muted: False), Ok(Nil))
}

// UTILS -----------------------------------------------------------------------

pub fn template(name: String, then next: fn(String) -> Cli(a)) -> Cli(a) {
  use env <- Cli
  let assert Ok(priv) = erlang.priv_directory("lustre_dev_tools")

  case simplifile.read(priv <> "/" <> name) {
    Ok(template) -> next(template).run(env)
    Error(error) -> #(env, Error(TemplateMissing(name, error)))
  }
}

// ENV -------------------------------------------------------------------------

pub fn get_config() -> Cli(Config) {
  use env <- Cli

  #(env, Ok(env.config))
}

pub fn get_name() -> Cli(String) {
  use env <- Cli

  #(env, Ok(env.config.name))
}

// FLAGS -----------------------------------------------------------------------

// pub fn get_flags() -> Cli(glint.Flags) {
//   use env <- Cli

//   #(env, Ok(env.flags))
// }

fn get(
  name: String,
  fallback: a,
  namespace: List(String),
  toml: fn(dict.Dict(String, tom.Toml), List(String)) -> Result(a, _),
  flag: Result(a, Nil),
) -> Cli(a) {
  use env <- Cli
  let toml_path = list.concat([["lustre-dev"], namespace, [name]])
  let value =
    result.or(
      result.nil_error(flag),
      result.nil_error(toml(env.config.toml, toml_path)),
    )
    |> result.unwrap(fallback)

  #(env, Ok(value))
}

pub fn get_int(
  name: String,
  fallback: Int,
  namespace: List(String),
  flag: Result(Int, Nil),
) -> Cli(Int) {
  get(name, fallback, namespace, tom.get_int, flag)
}

pub fn get_string(
  name: String,
  fallback: String,
  namespace: List(String),
  flag: Result(String, Nil),
) -> Cli(String) {
  get(name, fallback, namespace, tom.get_string, flag)
}

pub fn get_bool(
  name: String,
  fallback: Bool,
  namespace: List(String),
  flag: Result(Bool, Nil),
) -> Cli(Bool) {
  get(name, fallback, namespace, tom.get_bool, flag)
}
