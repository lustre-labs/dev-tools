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
import simplifile.{Execute, FilePermissions, Read, Write}

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
  case simplifile.is_file(path) {
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
        base <> "android-arm/-/android-arm-0.25.2.tgz",
        "B8DBDA0352E2EF679507DE0010C18705CBC7FCD5402ED1BD105B2707FCA4CCA1",
      ))
    "android", "arm64" ->
      Ok(#(
        base <> "android-arm64/-/android-arm64-0.25.2.tgz",
        "113FD6E8D1381C1EC329E0EAB3D95361211ABAA5231C16CE2BE89B54FA9EC1F2",
      ))
    "android", "x64" ->
      Ok(#(
        base <> "android-x64/-/android-x64-0.25.2.tgz",
        "0966C109B386D137C9092405B199C715C72B5CE8B9A1FA3D8C6B9D4C780FB9D8",
      ))

    "darwin", "aarch64" ->
      Ok(#(
        base <> "darwin-arm64/-/darwin-arm64-0.25.2.tgz",
        "D51E19BA63FE86A6FB6B70596AA0CA32362676274A50CFF6F4EF6E2DE02F4E4A",
      ))
    "darwin", "arm64" ->
      Ok(#(
        base <> "darwin-arm64/-/darwin-arm64-0.25.2.tgz",
        "D51E19BA63FE86A6FB6B70596AA0CA32362676274A50CFF6F4EF6E2DE02F4E4A",
      ))
    "darwin", "amd64" ->
      Ok(#(
        base <> "darwin-x64/-/darwin-x64-0.25.2.tgz",
        "DE2B564CB345FFAA6CD031A3C40920E8C3658A3321B864F53252F22D810380C5",
      ))
    "darwin", "x86_64" ->
      Ok(#(
        base <> "darwin-x64/-/darwin-x64-0.25.2.tgz",
        "DE2B564CB345FFAA6CD031A3C40920E8C3658A3321B864F53252F22D810380C5",
      ))

    "freebsd", "aarch64" ->
      Ok(#(
        base <> "freebsd-arm64/-/freebsd-arm64-0.25.2.tgz",
        "A8B16E6529F098CF7F8855CD2C5FBB21D740534181012AB819A4A569D9EACCDF",
      ))
    "freebsd", "amd64" ->
      Ok(#(
        base <> "freebsd-x64/-/freebsd-x64-0.25.2.tgz",
        "B2394FBF3B85390D5D3246C50192D2B1208D83DBF96796CDC67079C66FC0AA48",
      ))

    "linux", "arm" ->
      Ok(#(
        base <> "linux-arm/-/linux-arm-0.25.2.tgz",
        "9BF3844336FD30A1FCCBB6C0617572518A25E0716F98AC9E293B1D28AF97932D",
      ))
    "linux", "aarch64" ->
      Ok(#(
        base <> "linux-arm64/-/linux-arm64-0.25.2.tgz",
        "87D512B94D322CC3F008DC4EC5D9D47E82001DBEE692B825F332B157AFDF0282",
      ))
    "linux", "arm64" ->
      Ok(#(
        base <> "linux-arm64/-/linux-arm64-0.25.2.tgz",
        "87D512B94D322CC3F008DC4EC5D9D47E82001DBEE692B825F332B157AFDF0282",
      ))
    "linux", "ia32" ->
      Ok(#(
        base <> "linux-ia32/-/linux-ia32-0.25.2.tgz",
        "79BFB8B6B95F32C4EE8BBB21078A9E0F8DED3E148F69EAA690BCC0ABDB8D15F1",
      ))
    "linux", "x64" ->
      Ok(#(
        base <> "linux-x64/-/linux-x64-0.25.2.tgz",
        "52136A4B1F12F8B2567E0550B802F920593B012B22FEC64F46C99DF938466BBA",
      ))
    "linux", "x86_64" ->
      Ok(#(
        base <> "linux-x64/-/linux-x64-0.25.2.tgz",
        "52136A4B1F12F8B2567E0550B802F920593B012B22FEC64F46C99DF938466BBA",
      ))

    "netbsd", "x64" ->
      Ok(#(
        base <> "netbsd-x64/-/netbsd-x64-0.25.2.tgz",
        "0F1DC50A1A688DF5D3E41EDD9DB5B425D9B23566770029ACA4FB6C79D6F1806F",
      ))

    "openbsd", "arm64" ->
      Ok(#(
        base <> "openbsd-arm64/-/openbsd-arm64-0.25.2.tgz",
        "EB8B7F6BB6E56E869DA19573D5088991CA59C12870FC6180F249BA5D34163635",
      ))
    "openbsd", "x86_64" ->
      Ok(#(
        base <> "openbsd-x64/-/openbsd-x64-0.25.2.tgz",
        "196B5E6C9E4EC7895051E49CAF59F5720B9861FE87E66776961C296AE5217A18",
      ))

    "sunos", "x64" ->
      Ok(#(
        base <> "sunos-x64/-/sunos-x64-0.25.2.tgz",
        "409B456C7C341D9EF97621DA4AF84AD1CF0641B0546FE0D5D28A871C78BA2949",
      ))

    "win32", "arm64" ->
      Ok(#(
        base <> "win32-arm64/-/win32-arm64-0.25.2.tgz",
        "CAB0263FFB5CFC6B0609781EA0BF9565F0845C83802B86AB39E2107410911157",
      ))
    "win32", "ia32" ->
      Ok(#(
        base <> "win32-ia32/-/win32-ia32-0.25.2.tgz",
        "F62A2BE084752412CD3B6603668374A844DD3F8C8C37B3EB7382CE3C0F1F1C93",
      ))
    "win32", "x64" ->
      Ok(#(
        base <> "win32-x64/-/win32-x64-0.25.2.tgz",
        "C4D5FD874A44782032FC3BF5FE77D9CCE5D8C5B1DE51D9AE8EE68B40C2FC1ED2",
      ))
    "win32", "x86_64" ->
      Ok(#(
        base <> "win32-x64/-/win32-x64-0.25.2.tgz",
        "C4D5FD874A44782032FC3BF5FE77D9CCE5D8C5B1DE51D9AE8EE68B40C2FC1ED2",
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
  cmd.exec(
    "./build/.lustre/bin/esbuild",
    in: root,
    env: [#("NODE_PATH", "./node_modules")],
    with: options,
  )
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
