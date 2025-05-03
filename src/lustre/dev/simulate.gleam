// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/json.{type Json}
import gleam/list
import gleam/result
import lustre/dev/query.{type Query}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/vdom/events
import lustre/vdom/path

// TYPES -----------------------------------------------------------------------

/// A simulated [`lustre.App`](https://hexdocs.pm/lustre/lustre.html#App) ready
/// to be started. This module exposes constructor functions that mirrors those
/// provided by the main `lustre` module:
///
/// - [`simple`](#simple)
///
/// - [`application`](#application)
///
/// > **Note**: running a simulated app is not the same as running a real app!
/// > Any effects that would normally be run after update will be discarded. If
/// > you want to simulate messages coming from the outside world, you can use
/// > the [`message`](#message) or [`event`](#event) functions.
///
pub opaque type App(args, model, msg) {
  App(
    init: fn(args) -> #(model, Effect(msg)),
    update: fn(model, msg) -> #(model, Effect(msg)),
    view: fn(model) -> Element(msg),
  )
}

/// A running simulation of a Lustre application, produced by calling
/// [`start`](#start).This is similar to the [`Runtime`](https://hexdocs.pm/lustre/lustre.html#Runtime)
/// type and both DOM events and messages dispatched by effects can be simulated
/// using the [`event`](#event) and [`message`](#message) functions respectively.
///
/// Each simulated event returns an updated simulation, making it convenient to
/// pipe multiple events in sequence.
///
pub opaque type Simulation(model, msg) {
  Simulation(
    update: fn(model, msg) -> #(model, Effect(msg)),
    view: fn(model) -> Element(msg),
    history: List(Event(msg)),
    model: model,
    html: Element(msg),
  )
}

///
///
pub type Event(msg) {
  Dispatch(message: msg)
  Event(target: Query, name: String, data: Json)
  EventTargetNotFound(matching: Query)
  EventHandlerNotFound(target: Query, name: String)
}

//

///
///
pub fn simple(
  init init: fn(args) -> model,
  update update: fn(model, msg) -> model,
  view view: fn(model) -> Element(msg),
) -> App(args, model, msg) {
  App(
    init: fn(args) { #(init(args), effect.none()) },
    update: fn(model, msg) { #(update(model, msg), effect.none()) },
    view:,
  )
}

///
///
pub fn application(
  init init: fn(args) -> #(model, Effect(msg)),
  update update: fn(model, msg) -> #(model, Effect(msg)),
  view view: fn(model) -> Element(msg),
) -> App(args, model, msg) {
  App(init:, update:, view:)
}

///
///
pub fn component() {
  todo as "Simulate a component and its config"
}

//

///
///
pub fn start(app: App(args, model, msg), args: args) -> Simulation(model, msg) {
  let #(model, _) = app.init(args)
  let html = app.view(model)

  Simulation(update: app.update, view: app.view, history: [], model:, html:)
}

///
///
pub fn message(
  simulation: Simulation(model, msg),
  msg: msg,
) -> Simulation(model, msg) {
  let #(model, _) = simulation.update(simulation.model, msg)
  let html = simulation.view(model)
  let history = [Dispatch(message: msg), ..simulation.history]

  Simulation(..simulation, history:, model:, html:)
}

///
///
pub fn event(
  simulation: Simulation(model, msg),
  on query: Query,
  name event: String,
  data payload: List(#(String, Json)),
) -> Simulation(model, msg) {
  let path = path.root |> path.add(0, "")

  case query.find_path(in: simulation.html, matching: query, from: path) {
    Ok(#(_, path)) -> {
      let events = events.from_node(simulation.html)
      let data = json.object(payload)
      let #(_, result) =
        events.handle(
          events,
          path.to_string(path),
          event,
          data
            |> json.to_string
            |> json.parse(decode.dynamic)
            |> result.unwrap(erase(Nil)),
        )

      case result {
        Ok(msg) -> {
          let #(model, _) = simulation.update(simulation.model, msg)
          let html = simulation.view(model)
          let history = [
            Event(target: query, name: event, data:),
            ..simulation.history
          ]

          Simulation(..simulation, history:, model:, html:)
        }

        Error(_) ->
          Simulation(..simulation, history: [
            EventHandlerNotFound(target: query, name: event),
            ..simulation.history
          ])
      }
    }

    Error(_) ->
      Simulation(..simulation, history: [
        EventTargetNotFound(matching: query),
        ..simulation.history
      ])
  }
}

/// Introspect the current `view` of a running simulation. Typically you would
/// use this with a snapshot testing library like [`birdie`](https://hexdocs.pm/birdie/index.html)
/// or with the [`query`](./query.html) api to make assertions about the state of
/// the page.
///
pub fn html(simulation: Simulation(model, msg)) -> Element(msg) {
  simulation.html
}

/// Receive the current [`Event`](#Event) log of a running simulation. You can
/// use this to produce more detailed snapshots by also rendering the sequence of
/// events that produced the given view.
///
/// In addition to simulated DOM events and message dispatch, the event log will
/// also include entries for when the queried event target could not be found in
/// the view and cases where an event was fired but not handled by your application.
///
pub fn history(simulation: Simulation(model, msg)) -> List(Event(msg)) {
  simulation.history |> list.reverse
}

// UTILS -----------------------------------------------------------------------

@external(erlang, "gleam@function", "identity")
@external(javascript, "../../../gleam_stdlib/gleam/function.mjs", "identity")
fn erase(value: a) -> Dynamic
