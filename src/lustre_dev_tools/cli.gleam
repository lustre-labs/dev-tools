// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Dynamic}
import gleam/erlang
import gleam/io
import gleam/result
import gleam_community/ansi
import lustre_dev_tools/error.{type Error, TemplateMissing}
import simplifile
import spinner.{type Spinner}

// TYPES -----------------------------------------------------------------------

pub opaque type Env {
  Env(muted: Bool, spinner: SpinnerStatus)
}

type SpinnerStatus {
  Running(spinner: Spinner, message: String)
  Paused
}

///
///
pub type Cli(a, e) {
  Cli(run: fn(Env) -> #(Env, Result(a, e)))
}

//

///
///
pub fn run(step: Cli(a, Error)) -> Result(a, Error) {
  let env = Env(muted: False, spinner: Paused)
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

//

///
///
pub fn return(value: a) -> Cli(a, e) {
  use env <- Cli

  #(env, Ok(value))
}

pub fn from_result(result: Result(a, e)) -> Cli(a, e) {
  use env <- Cli

  #(env, result)
}

//

///
///
pub fn do(step: Cli(a, e), then next: fn(a) -> Cli(b, e)) -> Cli(b, e) {
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

pub fn in(value: fn() -> a) -> Cli(a, e) {
  use env <- Cli

  #(env, Ok(value()))
}

pub fn map(step: Cli(a, e), then next: fn(a) -> b) -> Cli(b, e) {
  use env <- Cli
  let #(env, result) = step.run(env)
  let result = result.map(result, next)

  #(env, result)
}

pub fn map_error(step: Cli(a, e), then next: fn(e) -> f) -> Cli(a, f) {
  use env <- Cli
  let #(env, result) = step.run(env)
  let result = result.map_error(result, next)

  #(env, result)
}

///
///
pub fn do_result(
  result: Result(a, e),
  then next: fn(a) -> Cli(b, e),
) -> Cli(b, e) {
  use env <- Cli

  case result {
    Ok(a) -> next(a).run(env)
    Error(e) -> #(env, Error(e))
  }
}

///
///
pub fn try(
  step: Result(a, x),
  catch recover: fn(x) -> e,
  then next: fn(a) -> Cli(b, e),
) -> Cli(b, e) {
  use env <- Cli

  case step {
    Ok(value) -> next(value).run(env)
    Error(error) -> {
      case env.spinner {
        Running(spinner, _message) -> spinner.stop(spinner)
        Paused -> Nil
      }

      #(env, Error(recover(error)))
    }
  }
}

//

///
///
pub fn log(message: String, then next: fn() -> Cli(a, e)) -> Cli(a, e) {
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

pub fn success(message: String, then next: fn() -> Cli(a, e)) -> Cli(a, e) {
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

pub fn mute() -> Cli(Nil, e) {
  use env <- Cli

  #(Env(..env, muted: True), Ok(Nil))
}

pub fn unmute() -> Cli(Nil, e) {
  use env <- Cli

  #(Env(..env, muted: False), Ok(Nil))
}

//

//

@external(erlang, "lustre_dev_tools_ffi", "exec")
pub fn exec(
  run command: String,
  with args: List(String),
  in in: String,
) -> Result(String, #(Int, String))

@external(erlang, "lustre_dev_tools_ffi", "get_cwd")
pub fn cwd() -> Result(String, Dynamic)

pub fn template(
  name: String,
  then next: fn(String) -> Cli(a, Error),
) -> Cli(a, Error) {
  use env <- Cli
  let assert Ok(priv) = erlang.priv_directory("lustre_dev_tools")

  case simplifile.read(priv <> "/" <> name) {
    Ok(template) -> next(template).run(env)
    Error(error) -> #(env, Error(TemplateMissing(name, error)))
  }
}
