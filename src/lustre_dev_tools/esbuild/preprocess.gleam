// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/bool
import gleam/list
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
    use source <- result.try(
      simplifile.read(path)
      |> result.map_error(CannotReadFile(_, path)),
    )

    let assert "./src/" <> module_path = path
    let out_path = filepath.join(out, module_path)
    let out_dir = filepath.directory_name(out_path)

    use _ <- result.try(
      simplifile.create_directory_all(out_dir)
      |> result.map_error(CannotCreateDirectory(_, out_dir)),
    )

    use _ <- result.try(
      simplifile.write(out_path, source)
      |> result.map_error(CannotWriteFile(_, out_path)),
    )

    Ok(Nil)
  })
}
