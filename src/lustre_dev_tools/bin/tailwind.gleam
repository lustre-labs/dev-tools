// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/bit_array
import gleam/bool
import gleam/crypto
import gleam/http
import gleam/http/request.{Request}
import gleam/http/response.{Response}
import gleam/httpc
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import lustre_dev_tools/error.{type Error}
import lustre_dev_tools/project
import lustre_dev_tools/system
import simplifile

//

///
///
pub fn download() {
  let os = system.os()
  let arch = system.arch()

  use hash <- result.try(
    list.key_find(hashes, #(os, arch))
    |> result.replace_error(error.UnsupportedPlatform(os, arch)),
  )

  let meta = project.meta()
  let dir = filepath.join(meta.root, ".lustre/bin")
  let out = filepath.join(dir, "tailwindcss")

  // If the binary already exists, check its hash to see if it's what we expect.
  // If it is, we can bail out early. Otherwise, we will download it again beacuse
  // the hash does not match.
  use <- bool.lazy_guard(
    case simplifile.read_bits(out) {
      Ok(bin) -> check_integrity(bin, hash) == Ok(Nil)
      Error(_) -> False
    },
    fn() {
      system.success("Tailwind already exists, skipping download.")
      Ok(Nil)
    },
  )

  use slug <- result.try(
    list.key_find(slugs, #(os, arch))
    |> result.replace_error(error.UnsupportedPlatform(os, arch)),
  )

  let request =
    Request(
      method: http.Get,
      scheme: http.Https,
      host: "github.com",
      port: option.None,
      path: "/tailwindlabs/tailwindcss/releases/download/v4.1.10/tailwindcss-"
        <> slug,
      query: option.None,
      headers: [],
      body: <<>>,
    )

  system.info(
    "Downloading Tailwind binary from: https://github.com/tailwindlabs/.../tailwindcss-"
    <> slug,
  )

  //
  use Response(status:, body:, ..) <- result.try(
    httpc.configure()
    |> httpc.follow_redirects(True)
    |> httpc.dispatch_bits(request)
    |> result.map_error(error.NetworkError),
  )

  //
  use <- bool.lazy_guard(status != 200, fn() {
    Error(error.UnexpectedResponse(
      downloading: "tailwind",
      status:,
      body: bit_array.to_string(body) |> result.unwrap(""),
    ))
  })

  // Once we've downloaded the binary, we need to check its integrity to guard
  // against MITM attacks or other issues that could cause the binary to be
  // corrupted or tampered with.
  use _ <- result.try(check_integrity(body, hash))

  system.success("Tailwind binary downloaded successfully.")

  //
  use _ <- result.try(
    simplifile.create_directory_all(dir)
    |> result.map_error(error.CouldNotWriteDirectoryOrFile(dir, _)),
  )

  use _ <- result.try(
    simplifile.write_bits(out, body)
    |> result.map_error(error.CouldNotWriteDirectoryOrFile(out, _)),
  )

  use _ <- result.try(
    simplifile.set_permissions_octal(out, 0o755)
    |> result.map_error(error.CouldNotWriteDirectoryOrFile(out, _)),
  )

  Ok(Nil)
}

fn check_integrity(bin: BitArray, expected: String) -> Result(Nil, Error) {
  let actual = bit_array.base16_encode(crypto.hash(crypto.Sha256, bin))

  case actual == expected {
    True -> Ok(Nil)
    False ->
      Error(error.CouldNotVerifyHash(for: "tailwind", expected:, actual:))
  }
}

/// Detect whether the project is using Tailwind. If it is, we will ensure the
/// basic minimal configuration is in place and download the Tailwind binary if
/// necessary.
///
pub fn detect() {
  let meta = project.meta()
  let legacy_config_path = filepath.join(meta.root, "tailwind.config.js")
  let config_path = filepath.join(meta.src, meta.name <> ".css")

  let has_legacy_config =
    legacy_config_path
    |> simplifile.is_file
    |> result.unwrap(False)

  let has_config =
    config_path
    |> simplifile.is_file
    |> result.unwrap(False)

  let has_valid_config =
    config_path
    |> simplifile.read
    |> result.map(string.contains(_, "@import \"tailwindcss"))
    |> result.unwrap(False)

  case has_legacy_config, has_valid_config {
    // We found a legacy Tailwind v3 config and no valid v4 config, but the user
    // does have a CSS file that we can use as the new config file. In this case,
    // we've detected that the project is using Tailwind so we should download it
    // and update the user's `{app_name}.css` file to include the Tailwind
    // directive.
    //
    // Tailwind also has a legacy `@config` directive for loading the old v3
    // config file, so we can use that to maintain continuity for the user until
    // they migrate to the new format.
    True, False if has_config -> {
      use _ <- result.try(download())
      use config <- result.try(
        simplifile.read(config_path)
        |> result.map_error(error.CouldNotReadFile(config_path, _)),
      )

      system.warn(
        "
Detected legacy Tailwind v3 config. Please note that Lustre Dev Tools uses Tailwind
v4. While your project may continue to work, please consult the Tailwind upgrade
guide to migrate your configuration: https://tailwindcss.com/docs/upgrade-guide
",
      )

      let config =
        "@import \"tailwindcss\";\n@config \"../tailwind.config.js\";\n"
        <> config

      system.info("Existing `" <> meta.name <> ".css` detected.")

      use _ <- result.try(
        simplifile.write(config_path, config)
        |> result.map_error(error.CouldNotWriteDirectoryOrFile(config_path, _)),
      )

      system.success("Tailwind config generated.")

      Ok(Nil)
    }

    // We found a legacy Tailwind v3 config and nothing valid for us to use as
    // the new config file. In this case we should download Tailwind and generate
    // a barebones `{app_name}.css` file that includes the Tailwind directive.
    //
    // Tailwind also has a legacy `@config` directive for loading the old v3
    // config file, so we can use that to maintain continuity for the user until
    // they migrate to the new format.
    True, False -> {
      use _ <- result.try(download())
      let config =
        "@import \"tailwindcss\";\n@config \"../tailwind.config.js\";\n"

      system.warn(
        "
Detected legacy Tailwind v3 config. Please note that Lustre Dev Tools uses Tailwind
v4. While your project may continue to work, please consult the Tailwind upgrade
guide to migrate your configuration: https://tailwindcss.com/docs/upgrade-guide
",
      )

      use _ <- result.try(
        simplifile.write(config_path, config)
        |> result.map_error(error.CouldNotWriteDirectoryOrFile(config_path, _)),
      )

      system.success("Tailwind config generated.")

      Ok(Nil)
    }

    // We found a legacy Tailwind v3 config but also a valid Tailwind v4 config.
    // In this case we won't edit any config at all, but we will still attempt
    // to download Tailwind.
    True, True -> {
      use _ <- result.try(download())

      system.success("Tailwind config detected.")

      Ok(Nil)
    }

    False, True -> {
      use _ <- result.try(download())

      system.success("Tailwind config detected.")

      Ok(Nil)
    }

    False, False if has_config -> {
      use _ <- result.try(download())
      use config <- result.try(
        simplifile.read(config_path)
        |> result.map_error(error.CouldNotReadFile(config_path, _)),
      )

      let config = "@import \"tailwindcss\";\n" <> config

      system.info("Existing `" <> meta.name <> ".css` detected.")

      use _ <- result.try(
        simplifile.write(config_path, config)
        |> result.map_error(error.CouldNotWriteDirectoryOrFile(config_path, _)),
      )

      system.success("Tailwind config generated.")

      Ok(Nil)
    }

    // There is no valid Tailwind config is this project, so we should just exit
    // this function.
    False, False -> Ok(Nil)
  }
}

// CONSTANTS -------------------------------------------------------------------

const slugs = [
  #(#("linux", "arm64"), "linux-arm64"),
  #(#("linux", "aarch64"), "linux-arm64"),
  #(#("linux", "x64"), "linux-x64"),
  #(#("linux", "x86_64"), "linux-x64"),
  #(#("win32", "x64"), "windows-x64.exe"),
  #(#("win32", "x86_64"), "windows-x64.exe"),
  #(#("darwin", "arm64"), "macos-arm64"),
  #(#("darwin", "aarch64"), "macos-arm64"),
  #(#("darwin", "x64"), "macos-x64"),
  #(#("darwin", "x86_64"), "macos-x64"),
]

const hashes = [
  #(
    #("linux", "arm64"),
    "67EB620BB404C2046D3C127DBF2D7F9921595065475E7D2D528E39C1BB33C9B6",
  ),
  #(
    #("linux", "aarch64"),
    "67EB620BB404C2046D3C127DBF2D7F9921595065475E7D2D528E39C1BB33C9B6",
  ),
  #(
    #("linux", "x64"),
    "0A85A3E533F2E7983BDB91C08EA44F0EAB3BECC275E60B3BAADDF18F71D390BF",
  ),
  #(
    #("linux", "x86_64"),
    "0A85A3E533F2E7983BDB91C08EA44F0EAB3BECC275E60B3BAADDF18F71D390BF",
  ),
  #(
    #("win32", "x64"),
    "5539346428771D8974AC63B68D1F477866BECECF615B3A14F2F197A36BDAAC33",
  ),
  #(
    #("win32", "x86_64"),
    "5539346428771D8974AC63B68D1F477866BECECF615B3A14F2F197A36BDAAC33",
  ),
  #(
    #("darwin", "arm64"),
    "F34A85A75B1F2DE2C7E4A9FBC4FB976E64A2780980E843DF87D9C13F555F4A4C",
  ),
  #(
    #("darwin", "aarch64"),
    "F34A85A75B1F2DE2C7E4A9FBC4FB976E64A2780980E843DF87D9C13F555F4A4C",
  ),
  #(
    #("darwin", "x64"),
    "47A130C5F639384456E0AC8A0D60B95D74906187314A4DBC37E7C1DDBEB713AE",
  ),
  #(
    #("darwin", "x86_64"),
    "47A130C5F639384456E0AC8A0D60B95D74906187314A4DBC37E7C1DDBEB713AE",
  ),
]
