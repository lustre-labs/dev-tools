// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/bool
import gleam/dynamic.{type Dynamic}
import gleam/io
import gleam/list
import gleam/result
import gleam/set
import gleam/string
import lustre_dev_tools/cli.{type Cli}
import lustre_dev_tools/project
import simplifile.{type FilePermissions, Execute, FilePermissions, Read, Write}

// COMMANDS --------------------------------------------------------------------

pub fn download(os: String, cpu: String) -> Cli(any, Nil, Error) {
  use <- cli.log("Downloading esbuild")

  let outdir = filepath.join(project.root(), "build/.lustre/bin")
  let outfile = filepath.join(outdir, "esbuild")

  case check_esbuild_exists(outfile) {
    True -> cli.success("Esbuild already installed!", fn() { cli.return(Nil) })
    False -> {
      use <- cli.log("Detecting platform")
      use url <- cli.do_result(get_download_url(os, cpu))

      use <- cli.log("Downloading from " <> url)
      use tarball <- cli.try(get_esbuild(url), NetworkError)

      use <- cli.log("Unzipping esbuild")
      use bin <- cli.try(unzip_esbuild(tarball), UnzipError)
      use _ <- cli.try(write_esbuild(bin, outdir, outfile), SimplifileError(
        _,
        outfile,
      ))
      use _ <- cli.try(set_filepermissions(outfile), SimplifileError(_, outfile))

      use <- cli.success("Esbuild installed!")
      cli.return(Nil)
    }
  }
}

pub fn bundle(
  input_file: String,
  output_file: String,
  minify: Bool,
  debug: Bool,
) -> Cli(any, Nil, Error) {
  use _ <- cli.do(download(get_os(), get_cpu()))
  use _ <- cli.try(project.build(), fn(_) { BuildError })
  use <- cli.log("Getting everything ready for tree shaking")

  let root = project.root()
  use _ <- cli.try(configure_node_tree_shaking(root), SimplifileError(_, root))

  let flags = [
    "--bundle",
    "--external:node:*",
    "--format=esm",
    "--define:DEBUG=" <> string.lowercase(bool.to_string(debug)),
    "--outfile=" <> output_file,
  ]
  let options = case minify {
    True -> [input_file, "--minify", ..flags]
    False -> [input_file, ..flags]
  }

  use <- cli.log("Bundling with esbuild")
  use _ <- cli.try(
    cli.exec(run: "./build/.lustre/bin/esbuild", in: root, with: options),
    fn(pair) { BundleError(pair.1) },
  )

  use <- cli.success("Bundle produced at `" <> output_file <> "`")
  cli.return(Nil)
}

pub fn serve(host: String, port: String, spa: Bool) -> Cli(any, Nil, Error) {
  use _ <- cli.do(download(get_os(), get_cpu()))
  let root = project.root()
  let flags = [
    "--serve=" <> host <> ":" <> port,
    "--servedir=" <> filepath.join(root, "build/.lustre"),
  ]

  let options = case spa {
    True -> [
      "--serve-fallback=" <> filepath.join(root, "build/.lustre/index.html"),
      ..flags
    ]
    False -> flags
  }

  use <- cli.success("Started dev server at http://" <> host <> ":" <> port)
  use _ <- cli.try(
    cli.exec(run: "./build/.lustre/bin/esbuild", in: root, with: options),
    fn(pair) { BundleError(pair.1) },
  )

  cli.return(Nil)
}

// STEPS -----------------------------------------------------------------------

fn check_esbuild_exists(path) {
  case simplifile.verify_is_file(path) {
    Ok(True) -> True
    Ok(False) | Error(_) -> False
  }
}

fn get_download_url(os, cpu) {
  let base = "https://registry.npmjs.org/@esbuild/"
  let path = case os, cpu {
    "android", "arm" -> Ok("android-arm/-/android-arm-0.19.10.tgz")
    "android", "arm64" -> Ok("android-arm64/-/android-arm64-0.19.10.tgz")
    "android", "x64" -> Ok("android-x64/-/android-x64-0.19.10.tgz")

    "darwin", "aarch64" -> Ok("darwin-arm64/-/darwin-arm64-0.19.10.tgz")
    "darwin", "amd64" -> Ok("darwin-arm64/-/darwin-arm64-0.19.10.tgz")
    "darwin", "arm64" -> Ok("darwin-arm64/-/darwin-arm64-0.19.10.tgz")
    "darwin", "x86_64" -> Ok("darwin-x64/-/darwin-x64-0.19.10.tgz")

    "freebsd", "arm64" -> Ok("freebsd-arm64/-/freebsd-arm64-0.19.10.tgz")
    "freebsd", "x64" -> Ok("freebsd-x64/-/freebsd-x64-0.19.10.tgz")

    "linux", "aarch64" -> Ok("linux-arm64/-/linux-arm64-0.19.10.tgz")
    "linux", "arm" -> Ok("linux-arm/-/linux-arm-0.19.10.tgz")
    "linux", "arm64" -> Ok("linux-arm64/-/linux-arm64-0.19.10.tgz")
    "linux", "ia32" -> Ok("linux-ia32/-/linux-ia32-0.19.10.tgz")
    "linux", "x64" -> Ok("linux-x64/-/linux-x64-0.19.10.tgz")
    "linux", "x86_64" -> Ok("linux-x64/-/linux-x64-0.19.10.tgz")

    "win32", "arm64" -> Ok("win32-arm64/-/win32-arm64-0.19.10.tgz")
    "win32", "ia32" -> Ok("win32-ia32/-/win32-ia32-0.19.10.tgz")
    "win32", "x64" -> Ok("win32-x64/-/win32-x64-0.19.10.tgz")
    "win32", "x86_64" -> Ok("win32-x64/-/win32-x64-0.19.10.tgz")

    "netbsd", "x64" -> Ok("netbsd-x64/-/netbsd-x64-0.19.10.tgz")
    "openbsd", "x64" -> Ok("openbsd-x64/-/openbsd-x64-0.19.10.tgz")
    "sunos", "x64" -> Ok("sunos-x64/-/sunos-x64-0.19.10.tgz")

    _, _ -> Error(UnknownPlatform(os, cpu))
  }

  result.map(path, string.append(base, _))
}

fn write_esbuild(bin, outdir, outfile) {
  let _ = simplifile.create_directory_all(outdir)

  simplifile.write_bits(outfile, bin)
}

fn set_filepermissions(file) {
  let permissions =
    FilePermissions(
      user: set.from_list([Read, Write, Execute]),
      group: set.from_list([Read, Execute]),
      other: set.from_list([Read, Execute]),
    )

  simplifile.set_permissions(file, permissions)
}

fn configure_node_tree_shaking(root) {
  // This whole chunk of code is to force tree shaking on dependencies that esbuild
  // has a habit of including because it thinks their imports might have side
  // effects.
  //
  // This is a really grim hack but it's the only way I've found to get esbuild to
  // ignore unused deps like `glint` that imports node stuff but aren't used in
  // app code.
  let force_tree_shaking = "{ \"sideEffects\": false }"
  let assert Ok(_) =
    simplifile.write(
      filepath.join(root, "build/dev/javascript/package.json"),
      force_tree_shaking,
    )
  let pure_deps = ["lustre", "glint", "simplifile"]

  list.try_each(pure_deps, fn(dep) {
    root
    |> filepath.join("build/dev/javascript/" <> dep)
    |> filepath.join("package.json")
    |> simplifile.write(force_tree_shaking)
  })
}

// ERROR HANDLING --------------------------------------------------------------

pub type Error {
  BuildError
  BundleError(message: String)
  NetworkError(Dynamic)
  SimplifileError(reason: simplifile.FileError, path: String)
  UnknownPlatform(os: String, cpu: String)
  UnzipError(Dynamic)
}

pub fn explain(error: Error) -> Nil {
  case error {
    BuildError -> project.explain(project.BuildError)

    BundleError(message) -> io.println("
I ran into an error while trying to create a bundle with esbuild:
" <> message)

    // TODO: Is there a better way to deal with this dynamic error?
    NetworkError(_dynamic) ->
      io.println(
        "
There was a network error!",
      )

    // TODO: this could give a better error for some common reason like Enoent.
    SimplifileError(reason, path) -> io.println("
I ran into the following error at path `" <> path <> "`: " <> string.inspect(
        reason,
      ) <> ".")

    UnknownPlatform(os, cpu) -> io.println("
I couldn't figure out the correct esbuild version for your
os (" <> os <> ") and cpu (" <> cpu <> ").")

    // TODO: Is there a better way to deal with this dynamic error?
    UnzipError(_dynamic) ->
      io.println(
        "
I couldn't unzip the esbuild executable!",
      )
  }
}

// EXTERNALS -------------------------------------------------------------------

@external(erlang, "lustre_dev_tools_ffi", "get_os")
fn get_os() -> String

@external(erlang, "lustre_dev_tools_ffi", "get_cpu")
fn get_cpu() -> String

@external(erlang, "lustre_dev_tools_ffi", "get_esbuild")
fn get_esbuild(url: String) -> Result(BitArray, Dynamic)

@external(erlang, "lustre_dev_tools_ffi", "unzip_esbuild")
fn unzip_esbuild(tarball: BitArray) -> Result(BitArray, Dynamic)
