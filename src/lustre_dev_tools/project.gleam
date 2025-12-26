// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/dict.{type Dict}
import gleam/result
import gleam/string
import lustre_dev_tools/error.{type Error}
import simplifile
import tom.{type Toml}

// TYPES -----------------------------------------------------------------------

///
///
pub type Project {
  Project(
    // Config
    name: String,
    options: Dict(String, Toml),
    // Directories
    root: String,
    src: String,
    dev: String,
    assets: String,
    bin: String,
    build: String,
    // Facts
    has_node_modules: Bool,
  )
}

// CONSTRUCTORS ----------------------------------------------------------------

///
///
pub fn initialise() -> Result(Project, Error) {
  let project = config()

  use _ <- result.try(case project.root {
    "./" -> Ok(Nil)
    "./" <> path | path -> Error(error.MustBeProjectRoot(path:))
  })

  use _ <- result.try(
    simplifile.create_directory_all(project.bin)
    |> result.map_error(error.CouldNotInitialiseDevTools),
  )

  use _ <- result.try(
    simplifile.create_directory_all(project.build)
    |> result.map_error(error.CouldNotInitialiseDevTools),
  )

  let gitignore_path = filepath.join(project.root, ".gitignore")

  use _ <- result.try(case simplifile.read(gitignore_path) {
    Ok(gitignore) ->
      case string.contains(gitignore, ".lustre") {
        True -> Ok(Nil)
        False ->
          simplifile.append(gitignore_path, {
            "\n#Added automatically by Lustre Dev Tools\n/.lustre\n/dist\n"
          })
          |> result.map_error(error.CouldNotInitialiseDevTools)
          |> result.replace(Nil)
      }

    Error(_) -> Ok(Nil)
  })

  Ok(project)
}

///
///
pub fn config() -> Project {
  let root = find_root("./")
  let bin = filepath.join(root, ".lustre/bin")
  let src = filepath.join(root, "src")
  let dev = filepath.join(root, "dev")
  let assets = filepath.join(root, "assets")
  let build = filepath.join(root, ".lustre/build")

  let has_node_modules =
    simplifile.is_directory(filepath.join(root, "node_modules"))
    |> result.unwrap(False)

  // These are safe to assert because we are guaranteed to be inside a Gleam
  // project if we're running this program! All Gleam projects must have a
  // `gleam.toml` with at least a `name` field, so we can safely assert all of
  // the following:
  //
  let assert Ok(toml) = simplifile.read(filepath.join(root, "gleam.toml"))
  let assert Ok(config) = tom.parse(toml)
  let assert Ok(name) = tom.get_string(config, ["name"])

  let options =
    tom.get_table(config, ["tools", "lustre"])
    |> result.unwrap(dict.new())

  Project(
    name:,
    root:,
    src:,
    dev:,
    assets:,
    bin:,
    build:,
    options:,
    has_node_modules:,
  )
}

// QUERIES ---------------------------------------------------------------------
fn find_root(path: String) -> String {
  case simplifile.is_file(filepath.join(path, "gleam.toml")) {
    Ok(True) -> path
    Ok(False) | Error(_) -> find_root(filepath.join(path, "../"))
  }
}

pub fn exists(project: Project, module: String) -> Bool {
  case simplifile.is_file(filepath.join(project.src, module <> ".gleam")) {
    Ok(True) -> True
    Ok(False) | Error(_) ->
      simplifile.is_file(filepath.join(project.dev, module <> ".gleam"))
      |> result.unwrap(False)
  }
}
