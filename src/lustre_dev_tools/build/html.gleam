import filepath
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import lustre/attribute.{type Attribute, attribute}
import lustre/element.{type Element}
import lustre/element/html
import lustre_dev_tools/project.{type Project}
import tom.{type Toml}

//

///
///
pub fn generate(
  project: Project,
  entry: String,
  tailwind_entry: Option(String),
  minify: Bool,
) -> String {
  let name = filepath.base_name(entry)
  let html =
    html.html([lang(project)], [
      html.head([], [
        html.meta([attribute.charset("utf-8")]),
        html.meta([
          attribute.name("viewport"),
          attribute.content("width=device-width, initial-scale=1"),
        ]),

        meta(project),

        title(project, entry),

        links(project),

        stylesheets(project),

        case tailwind_entry {
          Some(entry) ->
            html.link([
              attribute.rel("stylesheet"),
              attribute.href("/" <> filepath.base_name(entry) <> ".css"),
            ])

          None -> element.none()
        },

        scripts(project),

        html.script(
          [attribute.type_("module"), attribute.src("/" <> name <> ".js")],
          "",
        ),
      ]),

      body(project),
    ])

  // Lustre doesn't have a "to_readable_document_string" because typically the
  // built HTML document is only ever going to be served and not edited manually.
  //
  // Because we provide an explicit `--minify` it might be confusing to folks
  // if their HTML gets minified anyway though, so we call `to_readable_string`
  // and manually add the doctype.
  case minify {
    True -> element.to_document_string(html)
    False -> "<!doctype html>\n" <> element.to_readable_string(html)
  }
}

///
///
pub fn dev(
  project: Project,
  entry: String,
  tailwind_entry: Option(String),
) -> String {
  let html =
    html.html([lang(project)], [
      html.head([], [
        html.meta([attribute.charset("utf-8")]),
        html.meta([
          attribute.name("viewport"),
          attribute.content("width=device-width, initial-scale=1"),
        ]),

        meta(project),

        title(project, entry),

        links(project),

        stylesheets(project),

        case tailwind_entry {
          Some(entry) ->
            html.link([
              attribute.rel("stylesheet"),
              attribute.href("/" <> entry <> ".css"),
            ])

          None -> element.none()
        },

        scripts(project),

        html.script([attribute.src("/.lustre/server-hot-reload.js")], ""),

        case project.has_node_modules {
          True ->
            html.script(
              [attribute.type_("module"), attribute.src(entry <> ".dev.js")],
              "",
            )

          False ->
            html.script([attribute.type_("module")], {
              "
              import { main } from '/${name}/${entry}.mjs';

              main();
              "
              |> string.replace("${name}", project.name)
              |> string.replace("${entry}", entry)
            })
        },
      ]),

      body(project),
    ])

  element.to_document_string(html)
}

// ATTRIBUTES ------------------------------------------------------------------

fn lang(project: Project) -> Attribute(msg) {
  attribute.lang({
    tom.get_string(project.options, ["html", "lang"])
    |> result.unwrap("en")
  })
}

// ELEMENTS --------------------------------------------------------------------

fn title(project: Project, entry: String) -> Element(msg) {
  let name = filepath.base_name(entry)

  html.title([], {
    tom.get_string(project.options, ["html", "title"])
    |> result.unwrap(name)
  })
}

fn meta(project: Project) -> Element(msg) {
  element.fragment({
    tom.get_array(project.options, ["html", "meta"])
    |> result.map(list.filter_map(_, tom.as_table))
    |> result.map(
      list.map(_, {
        dict.fold(_, [], fn(attributes, key, toml) {
          case as_attribute(key, toml) {
            Ok(attribute) -> [attribute, ..attributes]
            Error(_) -> attributes
          }
        })
      }),
    )
    |> result.map(
      list.filter_map(_, fn(attributes) {
        case attributes {
          [_, ..] -> Ok(html.meta(attributes))
          [] -> Error(Nil)
        }
      }),
    )
    |> result.unwrap([])
  })
}

fn links(project: Project) -> Element(msg) {
  element.fragment({
    tom.get_array(project.options, ["html", "links"])
    |> result.map(list.filter_map(_, tom.as_table))
    |> result.map(
      list.map(_, {
        dict.fold(_, [], fn(attributes, key, toml) {
          case as_attribute(key, toml) {
            Ok(attribute) -> [attribute, ..attributes]
            Error(_) -> attributes
          }
        })
      }),
    )
    |> result.map(
      list.filter_map(_, fn(attributes) {
        case attributes {
          [_, ..] -> Ok(html.link(attributes))
          [] -> Error(Nil)
        }
      }),
    )
    |> result.unwrap([])
  })
}

fn stylesheets(project: Project) -> Element(msg) {
  element.fragment({
    tom.get_array(project.options, ["html", "stylesheets"])
    |> result.map(list.filter_map(_, tom.as_table))
    |> result.map(
      list.filter_map(_, fn(toml) {
        let href = tom.get_string(toml, ["href"])
        let content = tom.get_string(toml, ["content"])

        case href, content {
          Ok(href), _ ->
            Ok(html.link([attribute.href(href), attribute.rel("stylesheet")]))
          _, Ok(content) -> Ok(html.style([], content))
          _, _ -> Error(Nil)
        }
      }),
    )
    |> result.unwrap([])
  })
}

fn scripts(project: Project) -> Element(msg) {
  element.fragment({
    tom.get_array(project.options, ["html", "scripts"])
    |> result.map(list.filter_map(_, tom.as_table))
    |> result.map(
      list.filter_map(_, fn(toml) {
        let src = tom.get_string(toml, ["src"])
        let content = tom.get_string(toml, ["content"])
        let type_ = tom.get_string(toml, ["type"])

        case src, content, type_ {
          Ok(src), _, Ok(type_value) ->
            Ok(html.script(
              [attribute.src(src), attribute.type_(type_value)],
              "",
            ))
          Ok(src), _, _ -> Ok(html.script([attribute.src(src)], ""))

          _, Ok(content), Ok(type_value) ->
            Ok(html.script([attribute.type_(type_value)], content))
          _, Ok(content), _ -> Ok(html.script([], content))

          _, _, _ -> Error(Nil)
        }
      }),
    )
    |> result.unwrap([])
  })
}

fn as_attribute(key: String, toml: Toml) -> Result(Attribute(msg), Nil) {
  case tom.as_string(toml), tom.as_bool(toml) {
    Ok(value), _ -> Ok(attribute(key, value))
    _, Ok(True) -> Ok(attribute(key, ""))
    _, Ok(False) -> Error(Nil)
    _, _ -> Error(Nil)
  }
}

fn body(project: Project) -> Element(msg) {
  element.unsafe_raw_html("", "body", [], {
    tom.get_string(project.options, ["html", "body"])
    |> result.unwrap("<div id=\"app\"></div>")
  })
}
