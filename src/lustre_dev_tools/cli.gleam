// IMPORTS ---------------------------------------------------------------------

import gleam_community/ansi
import gleam/erlang
import gleam/result
import gleam/io
import gleam/string
import simplifile
import spinner.{type Spinner}

// TYPES -----------------------------------------------------------------------

pub opaque type Env {
  Env(spinner: SpinnerStatus)
}

type SpinnerStatus {
  Running(spinner: Spinner, message: String)
  Paused
}

///
///
pub type Cli(state, a, e) {
  Cli(run: fn(Env, state) -> #(Env, state, Result(a, e)))
}

//

///
///
pub fn run(step: Cli(state, a, e), with state: state) -> Result(a, e) {
  let env = Env(spinner: Paused)
  let #(env, _, result) = step.run(env, state)

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
pub fn return(value: a) -> Cli(state, a, e) {
  use env, state <- Cli

  #(env, state, Ok(value))
}

pub fn from_result(result: Result(a, e)) -> Cli(state, a, e) {
  use env, state <- Cli

  #(env, state, result)
}

//

///
///
pub fn do(
  step: Cli(state, a, e),
  then next: fn(a) -> Cli(state, b, e),
) -> Cli(state, b, e) {
  use env, state <- Cli
  let #(env, state, result) = step.run(env, state)

  case result {
    Ok(value) -> next(value).run(env, state)
    Error(error) -> {
      case env.spinner {
        Running(spinner, _message) -> spinner.stop(spinner)
        Paused -> Nil
      }

      #(env, state, Error(error))
    }
  }
}

pub fn map(step: Cli(state, a, e), then next: fn(a) -> b) -> Cli(state, b, e) {
  use env, state <- Cli
  let #(env, state, result) = step.run(env, state)
  let result = result.map(result, next)

  #(env, state, result)
}

pub fn map_error(
  step: Cli(state, a, e),
  then next: fn(e) -> f,
) -> Cli(state, a, f) {
  use env, state <- Cli
  let #(env, state, result) = step.run(env, state)
  let result = result.map_error(result, next)

  #(env, state, result)
}

///
///
pub fn do_result(
  result: Result(a, e),
  then next: fn(a) -> Cli(state, b, e),
) -> Cli(state, b, e) {
  use env, state <- Cli

  case result {
    Ok(a) -> next(a).run(env, state)
    Error(e) -> #(env, state, Error(e))
  }
}

///
///
pub fn try(
  step: Result(a, x),
  catch recover: fn(x) -> e,
  then next: fn(a) -> Cli(state, b, e),
) -> Cli(state, b, e) {
  use env, state <- Cli

  case step {
    Ok(value) -> next(value).run(env, state)
    Error(error) -> {
      case env.spinner {
        Running(spinner, _message) -> spinner.stop(spinner)
        Paused -> Nil
      }

      #(env, state, Error(recover(error)))
    }
  }
}

//

///
///
pub fn log(
  message: String,
  then next: fn() -> Cli(state, a, e),
) -> Cli(state, a, e) {
  use env, state <- Cli
  let env =
    Env(spinner: case env.spinner {
      Paused ->
        Running(
          spinner.new(message)
          |> spinner.with_frames(spinner.snake_frames)
          |> spinner.start,
          message,
        )

      Running(spinner, _) -> {
        spinner.set_text(spinner, message)
        Running(spinner, message)
      }
    })

  next().run(env, state)
}

pub fn success(
  message: String,
  then next: fn() -> Cli(state, a, e),
) -> Cli(state, a, e) {
  use env, state <- Cli
  let env =
    Env(spinner: case env.spinner {
      Paused -> Paused
      Running(spinner, _) -> {
        spinner.stop(spinner)
        Paused
      }
    })

  io.println("✅ " <> ansi.green(message))
  next().run(env, state)
}

//

///
///
pub fn get_state() -> Cli(state, state, e) {
  use env, state <- Cli

  #(env, state, Ok(state))
}

///
///
pub fn set_state(value: state) -> Cli(state, Nil, e) {
  use env, _ <- Cli

  #(env, value, Ok(Nil))
}

//

@external(erlang, "lustre_dev_tools_ffi", "exec")
pub fn exec(
  run command: String,
  with args: List(String),
  in in: String,
) -> Result(String, #(Int, String))

pub fn template(
  name: String,
  on_error: fn(String) -> e,
  then next: fn(String) -> Cli(state, a, e),
) -> Cli(state, a, e) {
  use env, state <- Cli
  let assert Ok(priv) = erlang.priv_directory("lustre_dev_tools")

  case simplifile.read(priv <> "/" <> name) {
    Ok(template) -> next(template).run(env, state)
    Error(error) -> #(
      env,
      state,
      Error(on_error(name <> ": " <> string.inspect(error))),
    )
  }
}
