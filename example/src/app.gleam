// IMPORTS ---------------------------------------------------------------------

import lustre
import lustre/element

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let assert Ok(_) =
    lustre.start(lustre.element(element.text("Hello, world")), "#app", Nil)
}
