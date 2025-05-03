// IMPORTS ---------------------------------------------------------------------

import gleam/list
import gleam/order
import gleam/string
import lustre/attribute
import lustre/element.{type Element}
import lustre/vdom/path.{type Path}
import lustre/vdom/vattr.{Attribute}
import lustre/vdom/vnode.{Element, Fragment, Text, UnsafeInnerHtml}

// TYPES -----------------------------------------------------------------------

/// A `Query` that describes how to locate certain elements in an `Element` tree.
/// You can pass a `Query` to functions like [`find`](#find) and [`find_all`](#find_all)
/// write tests that assert certain elements are present in your views.
///
pub opaque type Query {
  FindElement(matching: Selector)
  FindChild(of: Query, matching: Selector)
  FindDescendant(of: Query, matching: Selector)
}

///
///
pub opaque type Selector {
  All(of: List(Selector))
  Type(namespace: String, tag: String)
  HasAttribute(name: String, value: String)
  HasClass(name: String)
  HasStyle(name: String, value: String)
  Contains(content: String)
}

// SEARCHING -------------------------------------------------------------------

///
///
pub fn find(
  in root: Element(msg),
  matching query: Query,
) -> Result(Element(msg), Nil) {
  case find_path(in: root, matching: query, from: path.root) {
    Ok(#(element, _)) -> Ok(element)
    Error(_) -> Error(Nil)
  }
}

///
///
@internal
pub fn find_path(
  in root: Element(msg),
  matching query: Query,
  from path: Path,
) -> Result(#(Element(msg), Path), Nil) {
  case query {
    FindElement(matching: selector) ->
      case matches(root, selector) {
        True -> Ok(#(root, path))
        False -> find_in_children(root, query, path)
      }

    FindChild(of: parent, matching: selector) ->
      case find_path(in: root, matching: parent, from: path) {
        Ok(#(element, path)) -> find_direct_child(element, selector, path)
        Error(_) -> Error(Nil)
      }

    FindDescendant(of: parent, matching: selector) ->
      case find_path(in: root, matching: parent, from: path) {
        Ok(#(element, path)) -> find_descendant(element, selector, path)
        Error(_) -> Error(Nil)
      }
  }
}

fn find_in_children(
  element: Element(msg),
  query: Query,
  path: Path,
) -> Result(#(Element(msg), Path), Nil) {
  case element {
    Element(children:, ..) -> find_in_list(children, query, path, 0)
    Fragment(children:, ..) -> find_in_list(children, query, path, 1)
    UnsafeInnerHtml(..) -> Error(Nil)
    Text(..) -> Error(Nil)
  }
}

fn find_in_list(
  elements: List(Element(msg)),
  query: Query,
  path: Path,
  index: Int,
) -> Result(#(Element(msg), Path), Nil) {
  case elements {
    [] -> Error(Nil)
    [first, ..rest] -> {
      let child = path.add(path, index, first.key)

      case find_path(in: first, matching: query, from: child) {
        Ok(element) -> Ok(element)
        Error(_) -> find_in_list(rest, query, path, index + 1)
      }
    }
  }
}

fn find_direct_child(
  parent: Element(msg),
  selector: Selector,
  path: Path,
) -> Result(#(Element(msg), Path), Nil) {
  case parent {
    Element(children:, ..) -> find_matching_in_list(children, selector, path, 0)
    Fragment(children:, ..) ->
      find_matching_in_list(children, selector, path, 1)
    UnsafeInnerHtml(..) | Text(..) -> Error(Nil)
  }
}

fn find_matching_in_list(
  elements: List(Element(msg)),
  selector: Selector,
  path: Path,
  index: Int,
) -> Result(#(Element(msg), Path), Nil) {
  case elements {
    [] -> Error(Nil)
    [first, ..rest] ->
      case matches(first, selector) {
        True -> Ok(#(first, path.add(path, index, first.key)))
        False -> find_matching_in_list(rest, selector, path, index + 1)
      }
  }
}

fn find_descendant(
  parent: Element(msg),
  selector: Selector,
  path: Path,
) -> Result(#(Element(msg), Path), Nil) {
  case find_direct_child(parent, selector, path) {
    Ok(element) -> Ok(element)
    Error(_) ->
      case parent {
        Element(children:, ..) ->
          find_descendant_in_list(children, selector, path, 0)

        Fragment(children:, ..) ->
          find_descendant_in_list(children, selector, path, 1)

        UnsafeInnerHtml(..) | Text(..) -> Error(Nil)
      }
  }
}

fn find_descendant_in_list(
  elements: List(Element(msg)),
  selector: Selector,
  path: Path,
  index: Int,
) -> Result(#(Element(msg), Path), Nil) {
  case elements {
    [] -> Error(Nil)
    [first, ..rest] -> {
      case matches(first, selector) {
        True -> Ok(#(first, path.add(path, index, first.key)))
        False -> {
          let child = path.add(path, index, first.key)

          case find_descendant(first, selector, child) {
            Ok(element) -> Ok(element)
            Error(_) -> find_descendant_in_list(rest, selector, path, index + 1)
          }
        }
      }
    }
  }
}

///
///
pub fn find_all(
  in root: Element(msg),
  matching query: Query,
) -> List(Element(msg)) {
  case query {
    FindElement(matching: selector) ->
      case matches(root, selector) {
        True -> [root, ..find_all_in_children(root, query)]
        False -> find_all_in_children(root, query)
      }

    FindChild(of: parent, matching: selector) ->
      root
      |> find_all(matching: parent)
      |> list.flat_map(find_all_direct_children(_, selector))

    FindDescendant(of: parent, matching: selector) ->
      root
      |> find_all(matching: parent)
      |> list.flat_map(find_all_descendants(_, selector))
  }
}

fn find_all_in_children(
  element: Element(msg),
  query: Query,
) -> List(Element(msg)) {
  case element {
    Element(children:, ..) -> find_all_in_list(children, query)
    Fragment(children:, ..) -> find_all_in_list(children, query)
    UnsafeInnerHtml(..) -> []
    Text(..) -> []
  }
}

fn find_all_in_list(
  elements: List(Element(msg)),
  query: Query,
) -> List(Element(msg)) {
  case elements {
    [] -> []
    [first, ..rest] -> {
      let first_matches = find_all(in: first, matching: query)
      let rest_matches = find_all_in_list(rest, query)

      list.append(first_matches, rest_matches)
    }
  }
}

fn find_all_direct_children(
  parent: Element(msg),
  selector: Selector,
) -> List(Element(msg)) {
  case parent {
    Element(children:, ..) -> find_all_matching_in_list(children, selector)
    Fragment(children:, ..) -> find_all_matching_in_list(children, selector)
    UnsafeInnerHtml(..) | Text(..) -> []
  }
}

fn find_all_matching_in_list(
  elements: List(Element(msg)),
  selector: Selector,
) -> List(Element(msg)) {
  case elements {
    [] -> []
    [first, ..rest] ->
      case matches(first, selector) {
        True -> [first, ..find_all_matching_in_list(rest, selector)]
        False -> find_all_matching_in_list(rest, selector)
      }
  }
}

fn find_all_descendants(
  parent: Element(msg),
  selector: Selector,
) -> List(Element(msg)) {
  let direct_matches = find_all_direct_children(parent, selector)
  let descendant_matches = case parent {
    Element(children:, ..) -> find_all_descendants_in_list(children, selector)
    Fragment(children:, ..) -> find_all_descendants_in_list(children, selector)
    UnsafeInnerHtml(..) -> []
    Text(..) -> []
  }

  list.append(direct_matches, descendant_matches)
}

fn find_all_descendants_in_list(
  elements: List(Element(msg)),
  selector: Selector,
) -> List(Element(msg)) {
  case elements {
    [] -> []
    [first, ..rest] -> {
      let first_matches = find_all_descendants(first, selector)
      let rest_matches = find_all_descendants_in_list(rest, selector)

      list.append(first_matches, rest_matches)
    }
  }
}

fn matches(element: Element(msg), selector: Selector) -> Bool {
  case element, selector {
    _, All(of: selectors) -> list.all(selectors, matches(element, _))

    Element(namespace:, tag:, ..), Type(..)
    | UnsafeInnerHtml(namespace:, tag:, ..), Type(..)
    -> {
      namespace == selector.namespace && tag == selector.tag
    }

    Element(attributes:, ..), HasAttribute(name:, value: "")
    | UnsafeInnerHtml(attributes:, ..), HasAttribute(name:, value: "")
    ->
      list.any(attributes, fn(attribute) {
        case attribute {
          Attribute(..) -> attribute.name == name
          _ -> False
        }
      })

    Element(attributes:, ..), HasAttribute(name:, value:)
    | UnsafeInnerHtml(attributes:, ..), HasAttribute(name:, value:)
    -> list.contains(attributes, attribute.attribute(name, value))

    Element(attributes:, ..), HasClass(name)
    | UnsafeInnerHtml(attributes:, ..), HasClass(name)
    ->
      list.any(attributes, fn(attribute) {
        case attribute {
          Attribute(name: "class", value:, ..) ->
            value == name
            || string.starts_with(value, name <> " ")
            || string.ends_with(value, " " <> name)
            || string.contains(value, " " <> name <> " ")

          _ -> False
        }
      })

    Element(attributes:, ..), HasStyle(name:, value:)
    | UnsafeInnerHtml(attributes:, ..), HasStyle(name:, value:)
    -> {
      let rule = name <> ":" <> value <> ";"

      list.any(attributes, fn(attribute) {
        case attribute {
          Attribute(name: "style", value:, ..) -> string.contains(value, rule)
          _ -> False
        }
      })
    }

    Element(..), Contains(content:) -> {
      element
      |> text_content(False, "")
      |> string.contains(content)
    }

    _, _ -> False
  }
}

fn text_content(element: Element(msg), inline: Bool, content: String) -> String {
  case element {
    Fragment(..) ->
      list.fold(element.children, content, fn(content, child) {
        text_content(child, True, content)
      })

    Element(..) if !inline || element.namespace != "" ->
      list.fold(element.children, content, fn(content, child) {
        text_content(child, True, content)
      })

    Element(tag: "a", ..)
    | Element(tag: "abbr", ..)
    | Element(tag: "acronym", ..)
    | Element(tag: "b", ..)
    | Element(tag: "bdo", ..)
    | Element(tag: "big", ..)
    | Element(tag: "br", ..)
    | Element(tag: "button", ..)
    | Element(tag: "cite", ..)
    | Element(tag: "code", ..)
    | Element(tag: "dfn", ..)
    | Element(tag: "em", ..)
    | Element(tag: "i", ..)
    | Element(tag: "img", ..)
    | Element(tag: "input", ..)
    | Element(tag: "kbd", ..)
    | Element(tag: "label", ..)
    | Element(tag: "map", ..)
    | Element(tag: "object", ..)
    | Element(tag: "output", ..)
    | Element(tag: "q", ..)
    | Element(tag: "samp", ..)
    | Element(tag: "script", ..)
    | Element(tag: "select", ..)
    | Element(tag: "small", ..)
    | Element(tag: "span", ..)
    | Element(tag: "strong", ..)
    | Element(tag: "sub", ..)
    | Element(tag: "sup", ..)
    | Element(tag: "textarea", ..)
    | Element(tag: "time", ..)
    | Element(tag: "tt", ..)
    | Element(tag: "var", ..) ->
      list.fold(element.children, content, fn(content, child) {
        text_content(child, True, content)
      })

    Element(..) -> {
      // This doesn't include the semi-colon at the end of the rule because we
      // want to match any of the following:
      //
      //   display:inline
      //   display:inline-block
      //   display:inline grid
      //
      let rule = "display:inline"
      let is_inline =
        list.any(element.attributes, fn(attribute) {
          case attribute {
            Attribute(name: "style", value:, ..) -> string.contains(value, rule)
            _ -> False
          }
        })

      case is_inline {
        True ->
          list.fold(element.children, content, fn(content, child) {
            text_content(child, True, content)
          })

        False -> content
      }
    }

    Text(..) -> content <> element.content

    UnsafeInnerHtml(..) -> content
  }
}

// QUERIES ---------------------------------------------------------------------

///
///
pub fn element(matching selector: Selector) -> Query {
  FindElement(matching: selector)
}

///
///
pub fn child(of parent: Query, matching selector: Selector) -> Query {
  FindChild(of: parent, matching: selector)
}

///
///
pub fn descendant(of parent: Query, matching selector: Selector) -> Query {
  FindDescendant(of: parent, matching: selector)
}

// SELECTORS -------------------------------------------------------------------

///
///
pub fn and(first: Selector, second: Selector) -> Selector {
  case first {
    All(of: []) -> All(of: [second])
    All(of: others) -> All(of: [second, ..others])
    _ -> All(of: [first, second])
  }
}

///
///
pub fn tag(value: String) -> Selector {
  Type(namespace: "", tag: value)
}

///
///
pub fn namespaced(namespace: String, tag: String) -> Selector {
  Type(namespace:, tag:)
}

///
///
pub fn attribute(name: String, value: String) -> Selector {
  HasAttribute(name:, value:)
}

///
///
pub fn class(name: String) -> Selector {
  HasClass(name)
}

///
///
pub fn style(name: String, value: String) -> Selector {
  HasStyle(name:, value:)
}

///
///
pub fn id(name: String) -> Selector {
  HasAttribute(name: "id", value: name)
}

///
///
pub fn data(name: String, value: String) -> Selector {
  HasAttribute(name: "data-" <> name, value: value)
}

///
///
pub fn text(content: String) -> Selector {
  Contains(content:)
}

// PRETTY PRINTING -------------------------------------------------------------

/// Print a `Query` as a human-readable string similar to a CSS selector. This
/// function is primarily intended for debugging and testing purposes: for example,
/// you might use this to include the selector in a snapshot test for easier
/// review.
///
/// > **Note**: while similar, this function is not guaranteed to produce a valid
/// > CSS selector. Specifically, queries that use the [`text`](#text) selector
/// > will not be valid CSS selectors as they use the `:contains` pseudo-class,
/// > which is not part of the CSS spec!
///
pub fn to_readable_string(query: Query) -> String {
  case query {
    FindElement(matching: selector) -> selector_to_readable_string(selector)

    FindChild(of: parent, matching: selector) ->
      to_readable_string(parent)
      <> " > "
      <> selector_to_readable_string(selector)

    FindDescendant(of: parent, matching: selector) ->
      to_readable_string(parent) <> " " <> selector_to_readable_string(selector)
  }
}

fn selector_to_readable_string(selector: Selector) -> String {
  case selector {
    All(of: [])
    | Type(namespace: "", tag: "")
    | HasAttribute(name: "", ..)
    | HasClass(name: "")
    | HasStyle(name: "", ..)
    | HasStyle(value: "", ..)
    | Contains(content: "") -> ""

    All(of: selectors) ->
      selectors
      |> sort_selectors
      |> list.map(selector_to_readable_string)
      |> string.concat

    Type(namespace: "", tag:) -> tag
    Type(namespace:, tag:) -> namespace <> ":" <> tag
    HasAttribute(name: "id", value:) -> "#" <> value
    HasAttribute(name:, value: "") -> "[" <> name <> "]"
    HasAttribute(name:, value:) -> "[" <> name <> "=\"" <> value <> "\"]"
    HasClass(name:) -> "." <> name
    HasStyle(name:, value:) -> "[style*=\"" <> name <> ":" <> value <> "\"]"
    Contains(content:) -> ":contains(\"" <> content <> "\")"
  }
}

fn sort_selectors(selectors: List(Selector)) -> List(Selector) {
  use a, b <- list.sort({
    use selector <- list.flat_map(selectors)

    case selector {
      All(of: selectors) -> selectors
      _ -> [selector]
    }
  })

  case a, b {
    All(..), _ | _, All(..) -> panic as "`All` selectors should be flattened"

    Type(..), Type(..) ->
      case string.compare(a.namespace, b.namespace) {
        order.Eq -> string.compare(a.tag, b.tag)
        order -> order
      }

    Type(..), _ -> order.Lt
    _, Type(..) -> order.Gt

    HasAttribute(name: "id", ..), HasAttribute(name: "id", ..) ->
      string.compare(a.value, b.value)

    HasAttribute(name: "id", ..), _ -> order.Lt
    _, HasAttribute(name: "id", ..) -> order.Gt

    HasAttribute(..), HasAttribute(..) ->
      case string.compare(a.name, b.name) {
        order.Eq -> string.compare(a.value, b.value)
        order -> order
      }

    HasAttribute(..), _ -> order.Lt
    _, HasAttribute(..) -> order.Gt

    HasStyle(..), HasStyle(..) -> string.compare(a.name, b.name)
    HasStyle(..), _ -> order.Lt
    _, HasStyle(..) -> order.Gt

    HasClass(..), HasClass(..) -> string.compare(a.name, b.name)
    HasClass(..), _ -> order.Lt
    _, HasClass(..) -> order.Gt

    Contains(..), Contains(..) -> string.compare(a.content, b.content)
  }
}
