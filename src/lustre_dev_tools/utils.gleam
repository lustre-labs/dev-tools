import gleam/list
import gleam/result
import gleam/string
import term_size

pub fn term_width() -> Int {
  term_size.columns()
  |> result.unwrap(80)
}

pub fn shorten_url(url: String, to max_length: Int) -> String {
  let chunks = case url {
    "https://" <> rest -> ["https:/", ..string.split(rest, on: "/")]
    _ -> string.split(url, on: "/")
  }

  // We want to reduce the max length further to offset the `/` we'll
  // be adding back to join the remaining pieces.
  let max_length = max_length - list.length(chunks)
  case shorten(chunks, to: max_length) {
    Error(_) -> url
    Ok(#(left, right)) ->
      string.join(left, "/") <> "/.../" <> string.join(right, "/")
  }
}

/// Shortens a list of strings by removing pieces from the middle until it gets
/// down to the desired length, returning its two remaining halves.
/// Returns an error if it couldn't shorten the list.
///
fn shorten(
  strings: List(String),
  to max_length: Int,
) -> Result(#(List(String), List(String)), Nil) {
  let initial_length =
    list.fold(over: strings, from: 0, with: fn(acc, string) {
      acc + string.length(string)
    })

  // We want to divide the strings in two halves and remove items starting from
  // the middle going in both directions until we get to the desired length or
  // we can shorten it any further.
  let middle = list.length(strings) / 2
  let #(left, right) = list.split(strings, middle)
  // It's important we reverse the left part because we want to remove pieces
  // from its end, that is the part nearer to the middle of `strings`.
  let left = list.reverse(left)
  case do_shorten(left, right, False, Right, initial_length, max_length) {
    // Remember that the left part was reversed so that `do_shorten` could
    // remove items from its start! We have to reverse it back to normal.
    Ok(#(new_left, new_right)) -> Ok(#(list.reverse(new_left), new_right))
    Error(Nil) -> Error(Nil)
  }
}

type End {
  Left
  Right
}

/// Drops strings from the start of each list in turns until it shrinks it down
/// to the desired size.
/// If it could acutally shorten the lists it returns `Ok` wrapping them,
/// otherwise it returns `Error(Nil)`.
///
fn do_shorten(
  left: List(String),
  right: List(String),
  shortened: Bool,
  from: End,
  current_length: Int,
  max_length: Int,
) -> Result(#(List(String), List(String)), Nil) {
  case current_length <= max_length, left, right, from {
    // If we're already shorter than the maximum allowed length we don't shrink
    // the lists any further.
    True, _, _, _ ->
      case shortened {
        True -> Ok(#(left, right))
        False -> Error(Nil)
      }

    // If we're down to one or less chunks we can't shorten it any further.
    _, [], [_], _ | _, [], [], _ ->
      case shortened {
        True -> Ok(#(left, right))
        False -> Error(Nil)
      }

    // We always want to keep the rightmost chunk. So we make sure to
    // never drop it if it's the last one remaining on the right.
    _, left, [_] as right, Right ->
      do_shorten(left, right, shortened, Left, current_length, max_length)

    // Otherwise we remove a piece from the end specified by `end` until we
    // reach the desired size.
    _, [dropped, ..left], right, Left -> {
      let new_length = current_length - string.length(dropped)
      do_shorten(left, right, True, Right, new_length, max_length)
    }
    _, left, [dropped, ..right], Right -> {
      let new_length = current_length - string.length(dropped)
      do_shorten(left, right, True, Left, new_length, max_length)
    }

    // If we can't remove from the desired side we try shrinking further from
    // the other end.
    _, [], right, Left ->
      do_shorten([], right, shortened, Right, current_length, max_length)
    _, left, [], Right ->
      do_shorten(left, [], shortened, Left, current_length, max_length)
  }
}
