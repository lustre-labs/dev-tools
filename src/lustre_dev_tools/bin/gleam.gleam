import gleam/result
import lustre_dev_tools/error.{type Error}
import lustre_dev_tools/project.{type Project}
import lustre_dev_tools/system

pub fn build(_project: Project) -> Result(Nil, Error) {
  use _ <- result.try(
    system.run("gleam build --target javascript")
    |> result.map_error(error.ExternalCommandFailed("gleam", _)),
  )

  Ok(Nil)
}
