// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/bool
import gleam/regexp
import gleam/result
import gleam/string
import lustre_dev_tools/bin/bun
import lustre_dev_tools/error.{type Error}
import lustre_dev_tools/project.{type Project}
import simplifile

// TYPES -----------------------------------------------------------------------

pub type Detection {
  HasViableEntry
  HasTailwindEntry
  HasLegacyConfig
  Nothing
}

//

pub fn download(project: Project) -> Result(Nil, Error) {
  // 1. Install tailwind from npm. We need both the actual build tool and
  //    the CLI to interact with it. Running `bun.install` will also go and
  //    pull in bun itself if it's not already installed.
  use _ <- result.try(bun.install(project, "tailwindcss@4.1.11"))
  use _ <- result.try(bun.install(project, "@tailwindcss/cli@4.1.11"))

  // 2. Check if the user already has a CSS file that is - or could be - the
  //    entry for Tailwind.
  let tailwind_config = filepath.join(project.src, project.name <> ".css")
  let has_tailwind_config =
    tailwind_config
    |> simplifile.is_file
    |> result.unwrap(False)

  // 3. Detect if they have a legacy Tailwind v3 config file, which is
  //    deprecated but still supported in v4.
  let legacy_config = filepath.join(project.root, "tailwind.config.js")
  let has_legacy_config =
    legacy_config
    |> simplifile.is_file
    |> result.unwrap(False)

  // 4.
  use css <- result.try(case has_tailwind_config {
    False -> Ok("")
    True ->
      simplifile.read(tailwind_config)
      |> result.map_error(error.CouldNotReadFile(tailwind_config, _))
  })

  // 5. Check if the CSS file is already functioning as a Tailwind entry. If
  //    it is we assume the user has already set it up how they want and we
  //    don't modify it.
  let assert Ok(re) = regexp.from_string("^@import\\s+['\"]tailwindcss")
  use <- bool.guard(regexp.check(re, css), Ok(Nil))

  // 6. If we're going to go ahead and generate the Tailwind directives for
  //    the user, then we'll want to also include the `@config` directive if
  //    the user had a legacy v3 config. This probably means they're upgrading
  //    an older project and doing this means the upgrade will be a bit less
  //    painful.
  let directives = case has_legacy_config {
    False -> ["@import \"tailwindcss\";"]
    True -> ["@import \"tailwindcss\";", "@config \"../tailwind.config.js\";"]
  }

  let css =
    string.join(directives, "\n")
    |> string.append("\n")
    |> string.append(css)

  use _ <- result.try(
    simplifile.write(tailwind_config, css)
    |> result.map_error(error.CouldNotWriteFile(tailwind_config, _)),
  )

  Ok(Nil)
}

pub fn detect(project: Project) -> Result(Detection, Error) {
  // 1. Check if the user has a CSS file that is - or could be - the entry for
  //    Tailwind.
  let tailwind_config = filepath.join(project.src, project.name <> ".css")
  let has_tailwind_config =
    tailwind_config
    |> simplifile.is_file
    |> result.unwrap(False)

  // 2. Detect if they have a legacy Tailwind v3 config file, which is
  //    deprecated but still supported in v4.
  let legacy_config = filepath.join(project.root, "tailwind.config.js")
  let has_legacy_config =
    legacy_config
    |> simplifile.is_file
    |> result.unwrap(False)

  case has_tailwind_config {
    True -> {
      use css <- result.try(
        simplifile.read(tailwind_config)
        |> result.map_error(error.CouldNotReadFile(tailwind_config, _)),
      )

      let assert Ok(re) = regexp.from_string("^@import\\s+['\"]tailwindcss")
      case regexp.check(re, css) {
        True -> Ok(HasTailwindEntry)
        False -> Ok(HasViableEntry)
      }
    }
    False if has_legacy_config -> Ok(HasLegacyConfig)
    False -> Ok(Nothing)
  }
}

pub fn build(
  project project: Project,
  input in: String,
  output out: String,
  minify minify: Bool,
) -> Result(Nil, Error) {
  use _ <- result.try(bun.install(project, "tailwindcss@" <> version))
  use _ <- result.try(bun.install(project, "@tailwindcss/cli@" <> version))
  let path = "node_modules/@tailwindcss/cli/dist/index.mjs"

  use _ <- result.try(
    bun.run(project, path, [
      "-i",
      "../../" <> in,
      "-o",
      "../../" <> out,
      case minify {
        True -> "--minify"
        False -> ""
      },
    ]),
  )

  Ok(Nil)
}

// CONSTANTS -------------------------------------------------------------------

const version = "4.1.11"
