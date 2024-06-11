// IMPORTS ---------------------------------------------------------------------

import gleam/dict.{type Dict}
import gleam/dynamic.{type Decoder}
import lustre.{type App}
import lustre/attribute.{type Attribute}
import lustre/effect.{type Effect}
import lustre/element.{type Element}

// API -------------------------------------------------------------------------

pub const name = "your-component"

// This function is used when running registering the component using
// `lustre.register` and building the component as a standalone JavaScript bundle
// with `gleam run -m lustre/dev build component`.
pub fn component() -> App(Nil, Model, Msg) {
  lustre.component(init, update, view, on_attribute_change())
}

// This function will be used by your other view functions to render the component.
// Remember a component must be registered with `lustre.register` first!
pub fn element(attributes: List(Attribute(msg))) -> Element(Msg) {
  element.element(name, attributes, [])
}

// This function can be used to render a component on the server. It renders your
// component's tag name and inside that element it renders the initial view of
// your component.
pub fn prerender(attributes: List(Attribute(msg))) -> Element(Msg) {
  let #(model, _) = init(Nil)

  element.element(name, attributes, [view(model)])
}

// MODEL -----------------------------------------------------------------------

pub type Model {
  Model
}

fn init(_) -> #(Model, Effect(Msg)) {
  todo
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  Msg
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  todo
}

// This function sets up any attributes you want to listen for changes on. This
// will listen to changes on both HTML attributes as well as DOM node properties.
// Remember that attributes are always strings but properties can be any JavaScript
// value – you might find the deciphper package on hex useful for writing decoders.
fn on_attribute_change() -> Dict(String, Decoder(Msg)) {
  dict.from_list([])
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  todo
}
