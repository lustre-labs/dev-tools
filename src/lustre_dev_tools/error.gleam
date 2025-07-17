import gleam/httpc
import gleam/string
import lustre_dev_tools/system
import simplifile

pub type Error {
  CouldNotDownloadBunBinary(reason: httpc.HttpError)
  CouldNotExtractBunArchive(os: String, arch: String, version: String)
  CouldNotInitialiseDevTools(reason: simplifile.FileError)
  CouldNotLocateBunBinary(path: String)
  CouldNotReadFile(path: String, reason: simplifile.FileError)
  CouldNotSetFilePermissions(path: String, reason: simplifile.FileError)
  CouldNotStartFileWatcher(os: String, arch: String, version: String)
  CouldNotVerifyBunHash(expected: String, actual: String)
  CouldNotWriteFile(path: String, reason: simplifile.FileError)
  ExternalCommandFailed(command: String, reason: String)
  MissingRequiredFlag(name: String)
  UnknownBuildTool(name: String)
  UnknownIntegration(name: String)
  UnsupportedBunVersion(path: String, expected: String, actual: String)
  UnsupportedPlatform(os: String, arch: String)
  //
  Todo
}

pub fn explain(error: Error) -> String {
  case error {
    Todo -> "this error is not implemented yet"

    CouldNotDownloadBunBinary(reason:) ->
      "
I ran into a problem while trying to download the Bun binary. Here's the error I
got from the HTTP client:

  ${reason}

Make sure you're connected to the internet and that no firewall or proxy is
blocking connections to GitHub. If you think this is a bug, please open an issue
at

  https://github.com/lustre-labs/dev-tools/issues/new

Hint: you can provide a path to a local Bun binary by passing the `--bun-path`
flag or setting the `tools.lustre.bun_path` field in your `gleam.toml`.
      "
      |> string.replace("${reason}", string.inspect(reason))

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

Hint: you can provide a path to a local Bun binary by passing the `--bun-path`
flag or setting the `tools.lustre.bun_path` field in your `gleam.toml`.
      "
      |> string.replace("${os}", os)
      |> string.replace("${arch}", arch)
      |> string.replace("${version}", version)

    CouldNotLocateBunBinary(path:) ->
      "
I ran into a problem while trying to run the Bun binary at the following path:

  ${path}

If you are trying to use a local binary, make sure the path is correct and that
relative paths are relative to the current working directory. If you think this
is a bug, please open an issue at:

  https://github.com/lustre-labs/dev-tools/issues/new
      "
      |> string.replace("${path}", path)

    CouldNotInitialiseDevTools(reason:) ->
      "
I ran into a problem while setting up. I need to create some directories and
modify your `.gitignore` if you have one. Here's the error I got from the
file system:

  ${reason}

Make sure you have permissions to write to the current directory. If you think
this is a bug, please open an issue at:

  https://github.com/lustre-labs/dev-tools/issues/new
      "
      |> string.replace("${reason}", string.inspect(reason))

    CouldNotReadFile(path:, reason:) ->
      "
I ran into a problem while trying to read a file at the following path:

  ${path}

Here's the error I got from the file system:

  ${reason}

Make sure the file exists and that you have permissions to read it. If you think
this is a bug, please open an issue at:

  https://github.com/lustre-labs/dev-tools/issues/new
      "
      |> string.replace("${path}", path)
      |> string.replace("${reason}", string.inspect(reason))

    CouldNotSetFilePermissions(path:, reason:) ->
      "
I ran into a problem while trying to set the file permissions for the Bun binary
at:

  ${path}

Here's the error I got from the file system:

  ${reason}

Make sure you have the necessary permissions to write to this file. If you think
this is a bug, please open an issue at:

https://github.com/lustre-labs/dev-tools/issues/new
      "
      |> string.replace("${path}", path)
      |> string.replace("${reason}", string.inspect(reason))

    CouldNotStartFileWatcher(os:, arch:, version:) ->
      "
I ran into a problem while trying to start the file watcher. This might be due
to an incompatibility with your platform or a bug in the watcher code. Please
open an issue at:

  https://github.com/lustre-labs/dev-tools/issues/new

With the following information:

  - os: ${os}
  - arch: ${arch}
  - version: ${version}
      "
      |> string.replace("${os}", os)
      |> string.replace("${arch}", arch)
      |> string.replace("${version}", version)

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

Hint: you can provide a path to a local Bun binary by passing the `--bun-path`
flag or setting the `tools.lustre.bun_path` field in your `gleam.toml`.
      "
      |> string.replace("${expected}", expected)
      |> string.replace("${actual}", actual)

    CouldNotWriteFile(path:, reason:) ->
      "
I ran into a problem while trying to write a file at the following path:

  ${path}

Here's the error I got from the file system:

  ${reason}

Make sure you have permissions to write files in this directory. If you think
this is a bug, please open an issue at:

  https://github.com/lustre-labs/dev-tools/issues/new
      "
      |> string.replace("${path}", path)
      |> string.replace("${reason}", string.inspect(reason))

    ExternalCommandFailed(command:, reason:) ->
      "
I ran into a problem while trying to run the following command:

  ${command}

Here's the error I got from the command:

  ${reason}

Please open an issue at:

  https://github.com/lustre-labs/dev-tools/issues/new

With the following information:

  - command: ${command}
  - os: ${os}
  - arch: ${arch}
      "
      |> string.replace("${command}", command)
      |> string.replace("${reason}", reason)
      |> string.replace("${os}", system.detect_os())
      |> string.replace("${arch}", system.detect_arch())

    MissingRequiredFlag(name:) ->
      "
I'm missing at least one required flag to run this command. Please make sure you
provide the `--${name}` flag when running the command or configure your `gleam.toml`
to include the `tools.lustre.${name}` field. If you think this is a bug, please
open an issue at:

  https://github.com/lustre-labs/dev-tools/issues/new
      "
      |> string.replace("${name}", name)

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

    UnknownIntegration(name:) ->
      "
I don't know how to add the integration `${name}`. Currently I have integrations
for Bun and Tailwind. If you have suggestions for other integrations we could
support in the future, please open an issue at:

  https://github.com/lustre-labs/dev-tools/issues/new
      "
      |> string.replace("${name}", name)

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
  }
}
