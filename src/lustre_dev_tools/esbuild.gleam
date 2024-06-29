// IMPORTS ---------------------------------------------------------------------

import filepath
import gleam/bit_array
import gleam/crypto
import gleam/dynamic.{type Dynamic}
import gleam/result
import gleam/set
import lustre_dev_tools/cli.{type Cli}
import lustre_dev_tools/cmd
import lustre_dev_tools/error.{
  type Error, BundleError, CannotSetPermissions, CannotWriteFile, NetworkError,
  UnknownPlatform, UnzipError,
}
import lustre_dev_tools/project
import lustre_dev_tools/utils
import simplifile.{type FilePermissions, Execute, FilePermissions, Read, Write}

// COMMANDS --------------------------------------------------------------------

pub fn download(os: String, cpu: String) -> Cli(Nil) {
  use <- cli.log("Downloading esbuild")

  let outdir = filepath.join(project.root(), "build/.lustre/bin")
  let outfile = filepath.join(outdir, "esbuild")

  case check_esbuild_exists(outfile) {
    True -> cli.success("Esbuild already installed!", fn() { cli.return(Nil) })
    False -> {
      use <- cli.log("Detecting platform")
      use #(url, hash) <- cli.try(get_download_url_and_hash(os, cpu))

      // 17 is the size of "Downloading from " and 2 is the space taken by the
      // spinner.
      let max_url_size = utils.term_width() - 17 - 2
      let shortened_url = utils.shorten_url(url, to: max_url_size)
      use <- cli.log("Downloading from " <> shortened_url)
      use tarball <- cli.try(get_esbuild(url))

      use <- cli.log("Checking the downloaded tarball")
      use _ <- cli.try(check_esbuild_integrity(tarball, hash))

      use <- cli.log("Unzipping esbuild")
      use bin <- cli.try(unzip_esbuild(tarball))
      use _ <- cli.try(write_esbuild(bin, outdir, outfile))
      use _ <- cli.try(set_file_permissions(outfile))

      use <- cli.success("Esbuild installed!")
      cli.return(Nil)
    }
  }
}

pub fn bundle(input_file: String, output_file: String, minify: Bool) -> Cli(Nil) {
  use _ <- cli.do(download(get_os(), get_cpu()))
  use _ <- cli.try(project.build())
  use <- cli.log("Getting everything ready for tree shaking")

  let root = project.root()
  let flags = [
    "--bundle",
    "--external:node:*",
    "--format=esm",
    "--outfile=" <> output_file,
  ]
  let options = case minify {
    True -> [input_file, "--minify", ..flags]
    False -> [input_file, ..flags]
  }

  use <- cli.log("Bundling with esbuild")
  use _ <- cli.try(exec_esbuild(root, options))

  use <- cli.success("Bundle produced at `" <> output_file <> "`")
  cli.return(Nil)
}

// STEPS -----------------------------------------------------------------------

fn check_esbuild_exists(path) {
  case simplifile.verify_is_file(path) {
    Ok(True) -> True
    Ok(False) | Error(_) -> False
  }
}

/// Returns the appropriate url to download the esbuild binary and the expected
/// base16-encoded sha256 hash to check the downloaded binary's integrity.
///
fn get_download_url_and_hash(os, cpu) {
  let base = "https://registry.npmjs.org/@esbuild/"
  case os, cpu {
    "android", "arm" ->
      Ok(#(
        base <> "android-arm/-/android-arm-0.19.10.tgz",
        "545CF157B0E42E407AC1412F73876119414314D9E31982EBD1E9073336DA5365",
      ))
    "android", "arm64" ->
      Ok(#(
        base <> "android-arm64/-/android-arm64-0.19.10.tgz",
        "DFB0A873B1BB9698EF42561B9513FC4A7D8392CE84FBD44FC276883B82AB087E",
      ))
    "android", "x64" ->
      Ok(#(
        base <> "android-x64/-/android-x64-0.19.10.tgz",
        "B5D9D170F469BE483F3E16DA6033DFD064ED3DF788C6DC238BA6FE3232BF5653",
      ))

    "darwin", "aarch64" ->
      Ok(#(
        base <> "darwin-arm64/-/darwin-arm64-0.19.10.tgz",
        "8CEC451CBD47E228D2D0B740B62870471C22813348398061C06D829BC6610EA9",
      ))
    "darwin", "amd64" ->
      Ok(#(
        base <> "darwin-arm64/-/darwin-arm64-0.19.10.tgz",
        "8CEC451CBD47E228D2D0B740B62870471C22813348398061C06D829BC6610EA9",
      ))
    "darwin", "arm64" ->
      Ok(#(
        base <> "darwin-arm64/-/darwin-arm64-0.19.10.tgz",
        "8CEC451CBD47E228D2D0B740B62870471C22813348398061C06D829BC6610EA9",
      ))
    "darwin", "x86_64" ->
      Ok(#(
        base <> "darwin-x64/-/darwin-x64-0.19.10.tgz",
        "4AEC252E72CD56FD31341D960D8DCF0BEBFB858587061FE36B9047FD591C68A3",
      ))

    "freebsd", "aarch64" ->
      Ok(#(
        base <> "freebsd-arm64/-/freebsd-arm64-0.19.10.tgz",
        "739E9F6DD3121DB0CD03B70B14C4D17A1854970272B0F988BD035AA876BE254B",
      ))
    "freebsd", "amd64" ->
      Ok(#(
        base <> "freebsd-x64/-/freebsd-x64-0.19.10.tgz",
        "3E937C849B21B89244A8A62B473E36EEFE793F5BDF602BEDBB314DD33DDBE7EE",
      ))

    "linux", "aarch64" ->
      Ok(#(
        base <> "linux-arm64/-/linux-arm64-0.19.10.tgz",
        "D3523B8F7B2540BA5A15C4EE4C747B31DFDC496C7A8A3F3FB0ECCB3008647DB7",
      ))
    "linux", "arm" ->
      Ok(#(
        base <> "linux-arm/-/linux-arm-0.19.10.tgz",
        "99EEB37F5C1AB8750D9CAB6AB04469EF5CA32847B25E1461215276920AFB01B2",
      ))
    "linux", "arm64" ->
      Ok(#(
        base <> "linux-arm64/-/linux-arm64-0.19.10.tgz",
        "D3523B8F7B2540BA5A15C4EE4C747B31DFDC496C7A8A3F3FB0ECCB3008647DB7",
      ))
    "linux", "ia32" ->
      Ok(#(
        base <> "linux-ia32/-/linux-ia32-0.19.10.tgz",
        "3D69F7B90C62E6D94140355A92EDB15B8BFB934096C6E518BE41DAD6249BF38E",
      ))
    "linux", "x64" ->
      Ok(#(
        base <> "linux-x64/-/linux-x64-0.19.10.tgz",
        "73CA82A3C9049315027E60A50AF53C2ABFDE678BF66562B407FACA7FD3FAD6F4",
      ))
    "linux", "x86_64" ->
      Ok(#(
        base <> "linux-x64/-/linux-x64-0.19.10.tgz",
        "73CA82A3C9049315027E60A50AF53C2ABFDE678BF66562B407FACA7FD3FAD6F4",
      ))

    "win32", "arm64" ->
      Ok(#(
        base <> "win32-arm64/-/win32-arm64-0.19.10.tgz",
        "2D0EC6ED7C5BA6F2D99CBB1428C1FAABFA7D42E7435BC40474C5787DCD1FF37C",
      ))
    "win32", "ia32" ->
      Ok(#(
        base <> "win32-ia32/-/win32-ia32-0.19.10.tgz",
        "5BFBF08A8EDC16D53FE2103C68705DC3B4ABDFA6C44919B9602495ABA523BA46",
      ))
    "win32", "x64" ->
      Ok(#(
        base <> "win32-x64/-/win32-x64-0.19.10.tgz",
        "03EFF9A74ED7C72C8E4ACE85F6BFD2D097169D8D6E7D691AE1D7959F2912B785",
      ))
    "win32", "x86_64" ->
      Ok(#(
        base <> "win32-x64/-/win32-x64-0.19.10.tgz",
        "03EFF9A74ED7C72C8E4ACE85F6BFD2D097169D8D6E7D691AE1D7959F2912B785",
      ))

    "netbsd", "x64" ->
      Ok(#(
        base <> "netbsd-x64/-/netbsd-x64-0.19.10.tgz",
        "C8F6E2CB79B1DDC2AD42C0AE25FB2A769A989E36B917B231CF9847B683D6DD8D",
      ))
    "openbsd", "x64" ->
      Ok(#(
        base <> "openbsd-x64/-/openbsd-x64-0.19.10.tgz",
        "AFEBEAD35BB5A1B921C126E70E0D76CF04DB64FA53C60E0779816CFA9E1F9A11",
      ))
    "sunos", "x64" ->
      Ok(#(
        base <> "sunos-x64/-/sunos-x64-0.19.10.tgz",
        "B1E9F969433574BD43A293FA3A3C71C88B8C4CF841957DAAA2CF83A90ADAAB7E",
      ))

    _, _ -> Error(UnknownPlatform("esbuild", os, cpu))
  }
}

fn get_esbuild(url: String) -> Result(BitArray, Error) {
  do_get_esbuild(url)
  |> result.map_error(NetworkError)
}

fn unzip_esbuild(gzip: BitArray) -> Result(BitArray, Error) {
  do_unzip_esbuild(gzip)
  |> result.map_error(UnzipError)
}

fn check_esbuild_integrity(bin: BitArray, expected_hash: String) {
  let hash = crypto.hash(crypto.Sha256, bin)
  let hash_string = bit_array.base16_encode(hash)
  case hash_string == expected_hash {
    True -> Ok(Nil)
    False -> Error(error.InvalidEsbuildBinary)
  }
}

fn write_esbuild(
  bin: BitArray,
  outdir: String,
  outfile: String,
) -> Result(Nil, Error) {
  let _ = simplifile.create_directory_all(outdir)

  simplifile.write_bits(outfile, bin)
  |> result.map_error(CannotWriteFile(_, filepath.join(outdir, outfile)))
}

fn set_file_permissions(file: String) -> Result(Nil, Error) {
  let permissions =
    FilePermissions(
      user: set.from_list([Read, Write, Execute]),
      group: set.from_list([Read, Execute]),
      other: set.from_list([Read, Execute]),
    )

  simplifile.set_permissions(file, permissions)
  |> result.map_error(CannotSetPermissions(_, file))
}

fn exec_esbuild(root: String, options: List(String)) -> Result(String, Error) {
  cmd.exec("./build/.lustre/bin/esbuild", in: root, with: options)
  |> result.map_error(fn(pair) { BundleError(pair.1) })
}

// EXTERNALS -------------------------------------------------------------------

@external(erlang, "lustre_dev_tools_ffi", "get_os")
fn get_os() -> String

@external(erlang, "lustre_dev_tools_ffi", "get_cpu")
fn get_cpu() -> String

@external(erlang, "lustre_dev_tools_ffi", "get_esbuild")
fn do_get_esbuild(url: String) -> Result(BitArray, Dynamic)

@external(erlang, "lustre_dev_tools_ffi", "unzip_esbuild")
fn do_unzip_esbuild(tarball: BitArray) -> Result(BitArray, Dynamic)
