import gleam/int
import gleam/io
import gleam/result
import gleam/string
import gleam_community/ansi
import string_width.{Left, Size, Top}

///
@external(erlang, "system_ffi", "get_os")
pub fn os() -> String

///
@external(erlang, "system_ffi", "get_arch")
pub fn arch() -> String

//

///
pub fn info(message: String) -> Nil {
  format("", message) |> ansi.grey |> io.println
}

pub fn warn(message: String) -> Nil {
  format("", message) |> ansi.grey |> ansi.bold |> io.println_error
}

pub fn error(message: String) -> Nil {
  format("🚨", message) |> ansi.red |> io.println_error
}

pub fn success(message: String) -> Nil {
  format("✅", message) |> ansi.green |> io.println
}

fn format(tag: String, message: String) -> String {
  let term_size =
    string_width.get_terminal_size()
    |> result.unwrap(Size(rows: 24, columns: 80))

  let left_width = 2
  let left = string_width.align(tag, left_width, Left, with: " ")

  let gap = 2

  let right_width = int.min(80, term_size.columns - left_width - gap)
  let right =
    message
    |> string.trim
    |> string.replace("\n", " ")
    |> string_width.inline_styles
    |> string_width.limit(
      to: Size(rows: 10, columns: right_width),
      ellipsis: "…",
    )

  string_width.stack_horizontal([left, right], place: Top, gap:, with: " ")
}
