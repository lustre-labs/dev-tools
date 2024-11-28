// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/bool
import gleam/list
import gleam/option.{Some}
import gleam/regexp.{Match, Options}
import gleam/result
import gleam/string
import lustre_dev_tools/cli.{type Cli}
import lustre_dev_tools/error.{
  CannotCreateDirectory, CannotReadFile, CannotWriteFile,
}
import lustre_dev_tools/project
import simplifile

//

pub fn copy_deep_ffi() -> Cli(Nil) {
  use config <- cli.try(project.config())
  let root = project.root()
  let src = filepath.join(root, "src")
  let out = filepath.join(root, "build/dev/javascript/" <> config.name)

  let assert Ok(files) = simplifile.get_files(src)

  cli.from_result({
    use path <- list.try_each(files)
    use <- bool.guard(string.ends_with(path, ".gleam"), Ok(Nil))

    let assert "./src/" <> module_path = path
    let out_path = filepath.join(out, module_path)
    let out_dir = filepath.directory_name(out_path)

    use _ <- result.try(case filepath.extension(path) {
      // For all javascript source files, we will re-write any relative imports
      // to `*.gleam` modules until the Gleam compiler supports FFI code in folders
      // deeper than just `src/`
      Ok("js") | Ok("mjs") | Ok("ts") | Ok("mts") -> {
        use source <- result.try(
          simplifile.read(path)
          |> result.map_error(CannotReadFile(_, path))
          |> result.map(resolve_relative_gleam_imports(path, _)),
        )

        use _ <- result.try(
          simplifile.create_directory_all(out_dir)
          |> result.map_error(CannotCreateDirectory(_, out_dir)),
        )

        use _ <- result.try(
          simplifile.write(out_path, source)
          |> result.map_error(CannotWriteFile(_, out_path)),
        )

        Ok(Nil)
      }

      // All other files we will just copy them over, until the Gleam compiler
      // supports FFI code in folders deeper than just `src/`
      _ -> {
        use _ <- result.try(
          simplifile.create_directory_all(out_dir)
          |> result.map_error(CannotCreateDirectory(_, out_dir)),
        )

        use _ <- result.try(
          simplifile.copy_file(path, out_path)
          |> result.map_error(CannotWriteFile(_, out_path)),
        )

        Ok(Nil)
      }
    })

    Ok(Nil)
  })
}

fn can_resolve_relative_gleam_imports(path: String) -> Bool {
  case filepath.extension(path) {
    Ok("js") | Ok("mjs") | Ok("ts") | Ok("mts") -> True
    _ -> False
  }
}

fn resolve_relative_gleam_imports(path: String, source: String) -> String {
  use <- bool.guard(!can_resolve_relative_gleam_imports(path), source)
  let options = Options(case_insensitive: False, multi_line: True)
  let assert Ok(re) = regexp.compile("^import.+\"(\\..+)\";$", options)

  use source, match <- list.fold(regexp.scan(re, source), source)
  let assert Match(match, [Some(import_path)]) = match
  let resolved_import_path = string.replace(import_path, ".gleam", ".mjs")
  let resolved_import = string.replace(match, import_path, resolved_import_path)

  string.replace(source, match, resolved_import)
}
