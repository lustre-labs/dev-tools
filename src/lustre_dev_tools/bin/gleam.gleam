import gleam/result
import lustre_dev_tools/error.{type Error}
import lustre_dev_tools/project.{type Project}
import lustre_dev_tools/system

pub fn build(_project: Project) -> Result(Nil, Error) {
  use cmd <- result.try(
    system.find("gleam")
    |> result.replace_error(error.CouldNotLocateGleamBinary),
  )

  use _ <- result.try(
    system.run(cmd, ["build", "--target", "javascript"], [#("FORCE_COLOR", "1")])
    |> result.map_error(error.ExternalCommandFailed("gleam", _)),
  )

  Ok(Nil)
}
