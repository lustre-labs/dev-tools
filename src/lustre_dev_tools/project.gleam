// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/dict.{type Dict}
import gleam/result
import lustre_dev_tools/error.{type Error}
import simplifile
import tom.{type GetError, type Toml}

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
    bin: String,
    build: String,
    // Methods
    get_bool: fn(String) -> Result(Bool, GetError),
    get_string: fn(String) -> Result(String, GetError),
    get_int: fn(String) -> Result(Int, GetError),
  )
}

// CONSTRUCTORS ----------------------------------------------------------------

///
///
pub fn initialise() -> Result(Project, Error) {
  let project = config()

  use _ <- result.try(
    simplifile.create_directory_all(project.bin)
    |> result.map_error(error.CouldNotInitialiseDevTools),
  )

  use _ <- result.try(
    simplifile.create_directory_all(project.build)
    |> result.map_error(error.CouldNotInitialiseDevTools),
  )

  let gitignore = filepath.join(project.root, ".gitignore")

  case simplifile.is_file(gitignore) {
    Ok(True) ->
      simplifile.append(gitignore, "\n.lustre\n")
      |> result.map_error(error.CouldNotInitialiseDevTools)
      |> result.replace(project)

    Ok(False) -> Ok(project)
    Error(reason) -> Error(error.CouldNotInitialiseDevTools(reason:))
  }
}

///
///
pub fn config() -> Project {
  let root = find_root("./")
  let bin = filepath.join(root, ".lustre/bin")
  let src = filepath.join(root, "src")
  let build = filepath.join(root, ".lustre/build")

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
    bin:,
    build:,
    options:,
    get_bool: fn(name) { tom.get_bool(options, [name]) },
    get_string: fn(name) { tom.get_string(options, [name]) },
    get_int: fn(name) { tom.get_int(options, [name]) },
  )
}

// QUERIES ---------------------------------------------------------------------

fn find_root(path: String) -> String {
  case simplifile.is_file(filepath.join(path, "gleam.toml")) {
    Ok(True) -> path
    Ok(False) | Error(_) -> find_root(filepath.join(path, "../"))
  }
}
