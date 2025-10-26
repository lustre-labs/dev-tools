import gleam/httpc
import gleam/otp/actor
import gleam/string
import lustre_dev_tools/system
import simplifile

pub type Error {
  CouldNotDownloadBunBinary(reason: httpc.HttpError)
  CouldNotDownloadTailwindBinary(reason: httpc.HttpError)
  CouldNotExtractBunArchive(os: String, arch: String, version: String)
  CouldNotInitialiseDevTools(reason: simplifile.FileError)
  CouldNotLocateBunBinary(path: String)
  CouldNotLocateTailwindBinary(path: String)
  CouldNotLocateGleamBinary
  CouldNotReadFile(path: String, reason: simplifile.FileError)
  CouldNotSetFilePermissions(path: String, reason: simplifile.FileError)
  CouldNotStartDevServer(reason: actor.StartError)
  CouldNotStartFileWatcher(watcher: String, os: String, arch: String)
  CouldNotVerifyBunHash(expected: String, actual: String)
  CouldNotVerifyTailwindHash(expected: String, actual: String)
  CouldNotWriteFile(path: String, reason: simplifile.FileError)
  ExternalCommandFailed(command: String, reason: String)
  FailedToBuildProject(reason: String)
  MissingRequiredFlag(name: List(String))
  MustBeProjectRoot(path: String)
  ProxyInvalidTo
  ProxyMissingFrom
  ProxyMissingTo
  UnknownBuildTool(name: String)
  UnknownGleamModule(name: String)
  UnknownIntegration(name: String)
  UnsupportedBunVersion(path: String, expected: String, actual: String)
  UnsupportedPlatform(os: String, arch: String)
  UnsupportedTailwindVersion(path: String, expected: String, actual: String)
}

pub fn explain(error: Error) -> String {
  case error {
    // -------------------------------------------------------------------------
    CouldNotDownloadBunBinary(reason:) ->
      "
I ran into a problem while trying to download the Bun binary. Here's the error I
got from the HTTP client:

  ${reason}

Make sure you're connected to the internet and that no firewall or proxy is
blocking connections to GitHub.

Hint: you can provide a path to a local Bun binary by setting the
`tools.lustre.bin.bun` field in your `gleam.toml`. Use the string `\"system\"`
to use the Bun binary accessible in your system path.
      "
      |> string.replace("${reason}", string.inspect(reason))

    // -------------------------------------------------------------------------
    CouldNotDownloadTailwindBinary(reason:) ->
      "
I ran into a problem while trying to download the Tailwind binary. Here's the
error I got from the HTTP client:

  ${reason}

Make sure you're connected to the internet and that no firewall or proxy is
blocking connections to GitHub.

Hint: you can provide a path to a local Tailwind binary by setting the
`tools.lustre.bin.tailwind` field in your `gleam.toml`. Use the string `\"system\"`
to use the Tailwind binary accessible in your system path.
        "
      |> string.replace("${reason}", string.inspect(reason))

    // -------------------------------------------------------------------------
    CouldNotExtractBunArchive(os:, arch:, version:) ->
      "
I ran into an unexpected problem while trying to extract the Bun archive. This
might happen if the archive is corrupted or has changed format. Please open an
issue at:

  https://github.com/lustre-labs/dev-tools/issues/new

With the following information:

  - os: ${os}
  - arch: ${arch}
  - version: ${version}

Hint: you can provide a path to a local Tailwind binary by setting the
`tools.lustre.bin.bun` field in your `gleam.toml`. Use the string `\"system\"`
to use the Tailwind binary accessible in your system path.
      "
      |> string.replace("${os}", os)
      |> string.replace("${arch}", arch)
      |> string.replace("${version}", version)

    // -------------------------------------------------------------------------
    CouldNotLocateBunBinary(path:) ->
      "
I ran into a problem while trying to run the Bun binary at the following path:

  ${path}

If you are trying to use a local binary, make sure the path is correct and that
relative paths are relative to the project's root directory.
      "
      |> string.replace("${path}", path)

    // -------------------------------------------------------------------------
    CouldNotLocateTailwindBinary(path:) ->
      "
I ran into a problem while trying to run the Tailwind binary at the following
path:

  ${path}

If you are trying to use a local binary, make sure the path is correct and that
relative paths are relative to the current working directory.
      "
      |> string.replace("${path}", path)

    CouldNotLocateGleamBinary ->
      "
I ran into a problem while trying to run the Gleam binary.

Make sure Gleam is available in your current path.
      "

    // -------------------------------------------------------------------------
    CouldNotInitialiseDevTools(reason:) ->
      "
I ran into a problem while setting up. I need to create some directories and
modify your `.gitignore` if you have one. Here's the error I got from the
file system:

  ${reason}

Make sure you have permissions to write to the current directory.
      "
      |> string.replace("${reason}", string.inspect(reason))

    // -------------------------------------------------------------------------
    CouldNotReadFile(path:, reason:) ->
      "
I ran into a problem while trying to read a file at the following path:

  ${path}

Here's the error I got from the file system:

  ${reason}

Make sure the file exists and that you have permissions to read it.
      "
      |> string.replace("${path}", path)
      |> string.replace("${reason}", string.inspect(reason))

    // -------------------------------------------------------------------------
    CouldNotSetFilePermissions(path:, reason:) ->
      "
I ran into a problem while trying to set the file permissions for an integration
at:

  ${path}

Here's the error I got from the file system:

  ${reason}

Make sure you have the necessary permissions to write to this file.

Hint: you can provide a path to local binaries for Lustre to use instead by
adding the `tools.lustre.bin` table to your `gleam.toml`. Consult the TOML
reference on HexDocs for more information.
      "
      |> string.replace("${path}", path)
      |> string.replace("${reason}", string.inspect(reason))

    // -------------------------------------------------------------------------
    CouldNotStartFileWatcher(watcher:, os:, arch:) ->
      "
I ran into a problem while trying to start the file watcher. This might be due
to an incompatibility with your platform or a bug in the watcher code. Please
open an issue at:

  https://github.com/lustre-labs/dev-tools/issues/new

With the following information:

  - watcher: ${watcher}
  - os: ${os}
  - arch: ${arch}
      "
      |> string.replace("${watcher}", watcher)
      |> string.replace("${os}", os)
      |> string.replace("${arch}", arch)

    // -------------------------------------------------------------------------
    CouldNotVerifyBunHash(expected:, actual:) ->
      "
I ran into a problem while trying to verify the integrity of the Bun archive I
just downloaded. The expected hash was:

  ${expected}

But the actual hash was:

  ${actual}

This can happen if you have a proxy or firewall that interferes with the download.
If you think this is a bug, please open an issue at:

https://github.com/lustre-labs/dev-tools/issues/new

Hint: you can provide a path to a local Bun binary by setting the
`tools.lustre.bin.bun` field in your `gleam.toml`. Use the string `\"system\"`
to use the Bun binary accessible in your system path.
      "
      |> string.replace("${expected}", expected)
      |> string.replace("${actual}", actual)

    // -------------------------------------------------------------------------
    CouldNotVerifyTailwindHash(expected:, actual:) ->
      "
I ran into a problem while trying to verify the integrity of the Tailwind binary
I just downloaded. The expected hash was:

  ${expected}

But the actual hash was:

  ${actual}

This can happen if you have a proxy or firewall that interferes with the download.
If you think this is a bug, please open an issue at:

https://github.com/lustre-labs/dev-tools/issues/new

Hint: you can provide a path to a local Tailwind binary by setting the
`tools.lustre.bin.tailwind` field in your `gleam.toml`. Use the string `\"system\"`
to use the Tailwind binary accessible in your system path.
        "
      |> string.replace("${expected}", expected)
      |> string.replace("${actual}", actual)

    // -------------------------------------------------------------------------
    CouldNotWriteFile(path:, reason:) ->
      "
I ran into a problem while trying to write a file at the following path:

  ${path}

Here's the error I got from the file system:

  ${reason}

Make sure you have permissions to write files in this directory.
      "
      |> string.replace("${path}", path)
      |> string.replace("${reason}", string.inspect(reason))

    // -------------------------------------------------------------------------
    ExternalCommandFailed(command:, reason:) ->
      "
I ran into a problem while trying to run the following command:

  ${command}

Here's the error I got from the command:

  ${reason}
      "
      |> string.replace("${command}", command)
      |> string.replace("${reason}", reason)
      |> string.replace("${os}", system.detect_os())
      |> string.replace("${arch}", system.detect_arch())

    // -------------------------------------------------------------------------
    FailedToBuildProject(reason:) ->
      "
I ran into a problem while trying to build your application. Here's the error I
got while building:

  ${reason}

Make sure your Gleam code compiles without errors and any entry points point to
Gleam modules.
      "
      |> string.replace("${reason}", reason)

    // -------------------------------------------------------------------------
    MissingRequiredFlag(name:) ->
      "
I'm missing at least one required flag to run this command. Please make sure you
provide the `--${name}` flag when running the command or configure your `gleam.toml`
to include the `tools.lustre.${path}` field.
      "
      |> string.replace("${name}", string.join(name, "-"))
      |> string.replace("${path}", string.join(name, "."))

    // -------------------------------------------------------------------------
    MustBeProjectRoot(path:) ->
      "
I need to be run from the root directory of a Gleam project. I looked for a
`gleam.toml` and found one at:

  ${path}

Please run me from the directory that contains that file!
      "
      |> string.replace("${path}", path)

    // -------------------------------------------------------------------------
    ProxyInvalidTo ->
      "
I ran into a problem trying to set up the proxy you provided. The `to` URL
looks invalid. Please make sure you provide a valid URL for the `to` field.
      "

    // -------------------------------------------------------------------------
    ProxyMissingFrom ->
      "
I ran into a problem trying to set up the proxy you provided. The `from` field
is missing. Please make sure you provide a value for the `from` field like
`\"/api\"`.
      "

    // -------------------------------------------------------------------------
    ProxyMissingTo ->
      "
I ran into a problem trying to set up the proxy you provided. The `to` field
is missing. Please make sure you provide a value for the `to` field like
`\"http://localhost:3000\"`. This should be the full URL of the server you want
to proxy requests to.
      "

    // -------------------------------------------------------------------------
    UnknownBuildTool(name:) ->
      "
I ran into a problem trying to eject your project from Lustre Dev Tools. I don't
know how to eject for the build tool `${name}`. Currently I can generate the
config required for either Bun or Vite.

If you need to use a different build tool, please configure the project yourself.
If you think this is a bug, please open an issue at:

  https://github.com/lustre-labs/dev-tools/issues/new
      "
      |> string.replace("${name}", name)

    // -------------------------------------------------------------------------
    UnknownGleamModule(name:) ->
      "
I ran into a problem while trying to build your application. I couldn't find the
entry module you provided:

  ${name}

Make sure the module exists in your project and the name is correct. Gleam module
names are the full path from the `src` directory like `wibble/wobble/woo`.
      "
      |> string.replace("${name}", name)

    // -------------------------------------------------------------------------
    UnknownIntegration(name:) ->
      "
I don't know how to add the integration `${name}`. Currently I have integrations
for Bun and Tailwind. If you have suggestions for other integrations we could
support in the future, please open an issue at:

  https://github.com/lustre-labs/dev-tools/issues/new
      "
      |> string.replace("${name}", name)

    // -------------------------------------------------------------------------
    UnsupportedBunVersion(path:, expected:, actual:) ->
      "
I ran into a problem while trying to verify the version of the Bun binary at:

  ${path}

The expected version was:

  ${expected}

But the actual version was:

  ${actual}

If you are supplying a path to a local Bun binary, make sure it matches the
version I expect!
      "
      |> string.replace("${path}", path)
      |> string.replace("${expected}", expected)
      |> string.replace("${actual}", actual)
      |> string.trim

    // -------------------------------------------------------------------------
    UnsupportedTailwindVersion(path:, expected:, actual:) ->
      "
I ran into a problem while trying to verify the version of the Tailwind binary
at:

  ${path}

The expected version was:

  ${expected}

But the actual version was:

  ${actual}

If you are supplying a path to a local Bun binary, make sure it matches the
version I expect!
        "
      |> string.replace("${path}", path)
      |> string.replace("${expected}", expected)
      |> string.replace("${actual}", actual)
      |> string.trim

    // -------------------------------------------------------------------------
    UnsupportedPlatform(os:, arch:) ->
      "
Unfortunately, I don't support the platform you're running on. Currently, I
only support 64-bit Linux, macOS, and Windows systems. It would be helpful if
you could open an issue at:

  https://github.com/lustre-labs/dev-tools/issues/new

With the following information:

  - os: ${os}
  - arch: ${arch}

So we can consider how to support more users in the future. In the meantime, I
suggest you take a look at https://vite.dev or https://esbuild.github.io as
alternatives for building your Lustre applications.
      "
      |> string.replace("${os}", os)
      |> string.replace("${arch}", arch)

    // -------------------------------------------------------------------------
    CouldNotStartDevServer(reason:) ->
      "
I ran into a problem while trying to start the development server. Here's what I
got:

  ${reason}

Make sure the port you're trying to use is not already in use by another program.
      "
      |> string.replace("${reason}", string.inspect(reason))
  }
}
