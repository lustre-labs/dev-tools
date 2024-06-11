// IMPORTS ---------------------------------------------------------------------

import lustre/effect.{type Effect}
import lustre/element.{type Element}

// MODEL -----------------------------------------------------------------------

///
///
pub type Model {
  Model
}

pub fn init(data: Nil) -> #(Model, Effect(Msg)) {
  todo
}

// UPDATE ----------------------------------------------------------------------

pub opaque type Msg {
  Msg
}

pub fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  todo
}

// VIEW ------------------------------------------------------------------------

pub fn view(model: Model) -> Element(Msg) {
  todo
}

pub fn prerender(data: Nil) -> Element(Msg) {
  let #(model, _) = init(Nil)

  view(model)
}
