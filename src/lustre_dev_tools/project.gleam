// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/dict.{type Dict}
import gleam/list
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
    tests: String,
    assets: String,
    bin: String,
    build: String,
    // Facts
    has_package_json: Bool,
    has_node_modules: Bool,
    has_typescript: Bool,
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
  let src_files = simplifile.get_files(src) |> result.unwrap([])
  let dev = filepath.join(root, "dev")
  let dev_files = simplifile.get_files(dev) |> result.unwrap([])
  let tests = filepath.join(root, "test")
  let test_files = simplifile.get_files(tests) |> result.unwrap([])
  let assets = filepath.join(root, "assets")
  let build = filepath.join(root, ".lustre/build")

  let has_node_modules =
    simplifile.is_directory(filepath.join(root, "node_modules"))
    |> result.unwrap(False)

  let has_package_json =
    simplifile.is_file(filepath.join(root, "package.json"))
    |> result.unwrap(False)

  let has_tsconfig =
    simplifile.is_file(filepath.join(root, "tsconfig.json"))
    |> result.unwrap(False)

  let is_typescript_file = fn(path) {
    case filepath.extension(path) {
      Ok("ts") | Ok("cts") | Ok("mts") -> True
      _ -> False
    }
  }

  let has_typescript =
    has_tsconfig
    || list.any(src_files, is_typescript_file)
    || list.any(dev_files, is_typescript_file)
    || list.any(test_files, is_typescript_file)

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
    tests:,
    assets:,
    bin:,
    build:,
    options:,
    has_node_modules:,
    has_package_json:,
    has_typescript:,
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
      case simplifile.is_file(filepath.join(project.dev, module <> ".gleam")) {
        Ok(True) -> True
        Ok(False) | Error(_) ->
          simplifile.is_file(filepath.join(project.tests, module <> ".gleam"))
          |> result.unwrap(False)
      }
  }
}
