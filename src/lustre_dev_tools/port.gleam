import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/erlang/atom
import gleam/erlang/port.{type Port}
import gleam/erlang/process.{type Subject}
import gleam/json.{type Json}
import gleam/otp/actor.{type StartError}
import gleam/result

// TYPES -----------------------------------------------------------------------

///
///
pub opaque type Message {
  Send(String)
  Data(String)
  Exit(Int)
  Unknown
}

// CONSTRUCTORS ----------------------------------------------------------------

///
///
pub fn start(
  cmd program: String,
  with args: List(String),
  on_data handle_data: fn(Dynamic) -> Nil,
  on_unknown handle_unknown: fn() -> Nil,
) -> Result(Subject(Message), StartError) {
  actor.new_with_initialiser(1000, fn(self) {
    let port = do_start(program, args)
    let selector =
      process.new_selector()
      |> process.select(self)
      |> process.select_record(port, 1, fn(message) {
        message
        |> decode.run(decode_port_message())
        |> result.unwrap(Unknown)
      })

    actor.initialised(port)
    |> actor.selecting(selector)
    |> actor.returning(self)
    |> Ok
  })
  |> actor.on_message(fn(port, message) {
    case message {
      Send(data) -> {
        do_send(port, data)
        actor.continue(port)
      }

      Data(data) -> {
        case json.parse(data, decode.dynamic) {
          Ok(data) -> handle_data(data)
          Error(_) -> handle_unknown()
        }

        actor.continue(port)
      }

      Exit(0) -> actor.stop()
      Exit(_) -> actor.stop_abnormal("Port exited with code non-zero code")

      Unknown -> {
        handle_unknown()
        actor.continue(port)
      }
    }
  })
  |> actor.start
  |> result.map(fn(actor) { actor.data })
}

@external(erlang, "port_ffi", "start")
fn do_start(program: String, args: List(String)) -> Port

// MANIPULATIONS ---------------------------------------------------------------

///
///
pub fn send(port: Subject(Message), data: Json) -> Nil {
  process.send(port, Send(json.to_string(data) <> "\n"))
}

@external(erlang, "port_ffi", "send")
fn do_send(port: Port, data: String) -> Nil

// DECODERS --------------------------------------------------------------------

fn decode_port_message() -> Decoder(Message) {
  decode.at([1], {
    use tag <- decode.field(0, atom.decoder())

    let data_tag = atom.create("data")
    let exit_tag = atom.create("exit")

    case tag {
      _ if tag == data_tag ->
        decode.at([1], decode.string)
        |> decode.map(Data)

      _ if tag == exit_tag ->
        decode.at([1], decode.int)
        |> decode.map(Exit)

      _ -> decode.failure(Unknown, "")
    }
  })
}
