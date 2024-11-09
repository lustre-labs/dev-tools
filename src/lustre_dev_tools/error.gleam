// IMPORTS ---------------------------------------------------------------------

import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/otp/actor
import gleam/package_interface.{type Type, Fn, Named, Tuple, Variable}
import gleam/string
import glisten
import simplifile

// TYPES -----------------------------------------------------------------------

pub type Error {
  BuildError(reason: String)
  BundleError(reason: String)
  CannotCreateDirectory(reason: simplifile.FileError, path: String)
  CannotReadFile(reason: simplifile.FileError, path: String)
  CannotSetPermissions(reason: simplifile.FileError, path: String)
  CannotStartDevServer(reason: glisten.StartError, port: Int)
  CannotStartFileWatcher(reason: actor.StartError)
  CannotWriteFile(reason: simplifile.FileError, path: String)
  ComponentMissing(module: String)
  IncompleteProxy(missing: List(String))
  InternalError(message: String)
  InvalidProxyTarget(to: String)
  MainBadAppType(module: String, flags: Type, model: Type, msg: Type)
  MainMissing(module: String)
  MainTakesAnArgument(module: String, got: Type)
  ModuleMissing(module: String)
  NameIncorrectType(module: String, got: Type)
  NameMissing(module: String)
  NetworkError(Dynamic)
  TemplateMissing(name: String, reason: simplifile.FileError)
  UnknownFileError(simplifile.FileError)
  UnknownPlatform(binary: String, os: String, cpu: String)
  OtpTooOld(version: Int)
  UnzipError(Dynamic)
  InvalidEsbuildBinary
  InvalidTailwindBinary
}

// CONVERSIONS -----------------------------------------------------------------

pub fn explain(error: Error) -> String {
  case error {
    BuildError(reason) -> build_error(reason)
    BundleError(reason) -> bundle_error(reason)
    CannotCreateDirectory(reason, path) -> cannot_create_directory(reason, path)
    CannotReadFile(reason, path) -> cannot_read_file(reason, path)
    CannotSetPermissions(reason, path) -> cannot_set_permissions(reason, path)
    CannotStartDevServer(reason, port) -> cannot_start_dev_server(reason, port)
    CannotStartFileWatcher(reason) -> cannot_start_file_watcher(reason)
    CannotWriteFile(reason, path) -> cannot_write_file(reason, path)
    ComponentMissing(module) -> component_missing(module)
    IncompleteProxy(missing) -> incomplete_proxy(missing)
    InternalError(message) -> internal_error(message)
    InvalidProxyTarget(to) -> invalid_proxy_target(to)
    MainBadAppType(module, flags, model, msg) ->
      main_bad_app_type(module, flags, model, msg)
    MainMissing(module) -> main_missing(module)
    MainTakesAnArgument(module, got) -> main_takes_an_argument(module, got)
    ModuleMissing(module) -> module_missing(module)
    NameIncorrectType(module, got) -> name_incorrect_type(module, got)
    NameMissing(module) -> name_missing(module)
    NetworkError(error) -> network_error(error)
    TemplateMissing(name, reason) -> template_missing(name, reason)
    UnknownFileError(error) -> unknown_file_error(error)
    UnknownPlatform(binary, os, cpu) -> unknown_platform(binary, os, cpu)
    OtpTooOld(version) -> otp_too_old(version)
    UnzipError(error) -> unzip_error(error)
    InvalidEsbuildBinary -> invalid_esbuild_binary()
    InvalidTailwindBinary -> invalid_tailwind_binary()
  }
}

fn build_error(reason: String) -> String {
  let message =
    "
It looks like your project has some compilation errors that need to be addressed
before I can do anything. Here's the error message I got:

{reason}
"

  message
  |> string.replace("{reason}", reason)
}

fn bundle_error(reason: String) -> String {
  let message =
    "
I ran into an unexpected issue while trying to bundle your project with esbuild.
Here's the error message I got:

    {reason}

If you think this is a bug, please open an issue at
https://github.com/lustre-labs/dev-tools/issues/new with some details about what
you were trying to do when you ran into this issue.
"

  message
  |> string.replace("{reason}", reason)
}

fn cannot_create_directory(reason: simplifile.FileError, path: String) -> String {
  let message =
    "
I ran into an error while trying to create the following directory:

    {path}

Here's the error message I got:

    {reason}

If you think this is a bug, please open an issue at
https://github.com/lustre-labs/dev-tools/issues/new with some details about what
you were trying to do when you ran into this issue.
"

  message
  |> string.replace("{path}", path)
  |> string.replace("{reason}", string.inspect(reason))
}

fn cannot_read_file(reason: simplifile.FileError, path: String) -> String {
  let message =
    "
I ran into an error while trying to read the following file:

    {path}

Here's the error message I got:

    {reason}

If you think this is a bug, please open an issue at
https://github.com/lustre-labs/dev-tools/issues/new with some details about what
you were trying to do when you ran into this issue.
"

  message
  |> string.replace("{path}", path)
  |> string.replace("{reason}", string.inspect(reason))
}

fn cannot_set_permissions(reason: simplifile.FileError, path: String) -> String {
  let message =
    "
I ran into an error while trying to set the permissions on the following file:

    {path}

Here's the error message I got:

    {reason}

If you think this is a bug, please open an issue at
https://github.com/lustre-labs/dev-tools/issues/new with some details about what
you were trying to do when you ran into this issue.
"

  message
  |> string.replace("{path}", path)
  |> string.replace("{reason}", string.inspect(reason))
}

fn cannot_start_dev_server(reason: glisten.StartError, port: Int) -> String {
  case reason {
    glisten.AcceptorFailed(process.Abnormal(message)) ->
      case string.contains(message, "Eaddrinuse") {
        True -> cannot_start_dev_server_port_in_use_message(port)
        False -> cannot_start_dev_server_default_message(reason)
      }
    _ -> cannot_start_dev_server_default_message(reason)
  }
}

fn cannot_start_dev_server_default_message(reason: glisten.StartError) -> String {
  let message =
    "
I ran into an error while trying to start the development server. Here's the
error message I got:

    {reason}

Please open an issue at https://github.com/lustre-labs/dev-tools/issues/new with
some details about what you were trying to do when you ran into this issue.
"

  message
  |> string.replace("{reason}", string.inspect(reason))
}

fn cannot_start_dev_server_port_in_use_message(port: Int) -> String {
  let message =
    "
I ran into an error while trying to start the development server:
port {port} is already in use.
You can change the port to start the dev server on using the `--port` flag.
"

  message
  |> string.replace("{port}", int.to_string(port))
}

fn cannot_start_file_watcher(reason: actor.StartError) -> String {
  let message =
    "
I ran into an error while trying to start the file watcher used for live reloading.
Here's the error message I got:

    {reason}

Please open an issue at https://github.com/lustre-labs/dev-tools/issues/new with
some details about what you were trying to do when you ran into this issue.
"

  message
  |> string.replace("{reason}", string.inspect(reason))
}

fn cannot_write_file(reason: simplifile.FileError, path: String) -> String {
  let message =
    "
I ran into an error while trying to write the following file:

    {path}

Here's the error message I got:

    {reason}

If you think this is a bug, please open an issue at
https://github.com/lustre-labs/dev-tools/issues/new with some details about what
you were trying to do when you ran into this issue.
"

  message
  |> string.replace("{path}", path)
  |> string.replace("{reason}", string.inspect(reason))
}

fn component_missing(module: String) -> String {
  let message =
    "
I couldn't find a valid component definition in the following module:

    {module}

To bundle a component, the module should have a public function that returns a
Lustre `App`. Try adding a function like this:

    pub const name: String = \"my-component\"

    pub fn component() -> App(Nil, Model, Msg) {
      lustre.component(init, update, view, on_attribute_change())
    }
"

  message
  |> string.replace("{module}", module)
}

fn incomplete_proxy(missing: List(String)) -> String {
  let message =
    "
I'm missing some information needed to proxy requests from the development server.
The following keys are missing:

    {missing}

You can provide the missing information either as flags when starting the
development server, or by adding a `proxy` key to the `lustre-dev` section of
your `gleam.toml`.

To pass the information as flags, you should start the development server like
this:

    gleam run -m lustre/dev start --proxy-from=/api --proxy-to=http://localhost:4000/api

To add the information to your `gleam.toml`, make sure it looks something like
this:

    [lustre-dev.start]
    proxy = { from = \"/api\", to = \"http://localhost:4000/api\" }
"

  message
  |> string.replace("{missing}", string.join(missing, ", "))
}

fn internal_error(info: String) -> String {
  let message =
    "
Oops, it looks like I ran into an unexpected error while trying to do something.
Please open an issue at https://github.com/lustre-labs/dev-tools/issues/new with
the following message:

    {info}
"

  message
  |> string.replace("{info}", info)
}

pub fn invalid_proxy_target(to: String) -> String {
  let message =
    "
I ran into an issue reading your proxy configuration. The URI you provided as the
target for the proxy is invalid:

    {to}

Please make sure the URI is valid and try again. If you think this is a bug,
please open an issue at https://github.com/lustre-labs/dev-tools/issues/new
"

  message
  |> string.replace("{to}", to)
}

fn main_bad_app_type(
  module: String,
  flags: Type,
  model: Type,
  msg: Type,
) -> String {
  let message =
    "
I don't know how to serve the Lustre app returned from the `main` function in the
following module:

    {module}

I worked out your app type to be:

    App({flags}, {model}, {msg})

I need your app's flags type to either be `Nil` or a type variable like `a`. Your
`main` function should look something like this:

    pub fn main() -> App(Nil, {model}, {msg}) {
      lustre.application(init, update, view)
    }

I don't know how to produce flags of type `{flags}`! If this is intentional and
you want to provide your own flags, try modifying your `main` function to look
like this:

    pub fn main() -> Nil {
      let app = lustre.application(init, update, view)
      let flags = todo // provide your flags here
      let assert Ok() = lustre.run(app, \"#app\", flags)

      Nil
    }
"

  message
  |> string.replace("{module}", module)
  |> string.replace("{flags}", pretty_type(flags))
  |> string.replace("{model}", pretty_type(model))
  |> string.replace("{msg}", pretty_type(msg))
}

fn main_missing(module: String) -> String {
  let message =
    "
I couldn't find a `main` function in the following module:

    {module}

Is the module path correct? Your app's `main` function is the entry point we use
to build and start your app. It should look something like this:

    pub fn main() -> App(Nil, Model, Msg) {
      lustre.application(init, update, view)
    }
"

  message
  |> string.replace("{module}", module)
}

fn main_takes_an_argument(module: String, got: Type) -> String {
  let message =
    "
I ran into a problem trying to serve your Lustre app in the following module:

    {module}

I worked out the type of your `main` function to be:

    {got}

The `main` function should not take any arguments because I don't know how to
provide them! Your `main` function should look something like this:

    pub fn main() -> App(Nil, Model, Msg) {
      lustre.application(init, update, view)
    }
"

  message
  |> string.replace("{module}", module)
  |> string.replace("{got}", pretty_type(got))
}

fn module_missing(module: String) -> String {
  let message =
    "
I couldn't find the following module:

    {module}

Make sure the module path is correct and also the module is not included in the
`internal_modules` list in your `gleam.toml`.

The Gleam compiler currently has a bug with it's package-interface export that
will affect lustre_dev_tools. You can find more information about that bug here:

    https://github.com/gleam-lang/gleam/issues/2898

If you know the above module exists, try running `gleam clean` and then run the
dev tools again. If you think this is a bug, please open an issue on GitHub with
some details about what you were trying to do when you ran into this issue:

    https://github.com/lustre-labs/dev-tools/issues/new
"

  message
  |> string.replace("{module}", module)
}

fn name_incorrect_type(module: String, got: Type) -> String {
  let message =
    "
I ran into a problem trying to bundle the component in the following module:

    {module}

The type of the `name` constant isn't what I expected. I worked out the type to
be:

    {got}

The `name` constant should be a string. Make sure it's defined like this:

    pub const name: String = \"my-component\"

If you think this is a bug, please open an issue at
https://github.com/lustre-labs/dev-tools/issues/new with some details about what
you were trying to do when you ran into this issue.
"

  message
  |> string.replace("{module}", module)
  |> string.replace("{got}", pretty_type(got))
}

fn name_missing(module: String) -> String {
  let message =
    "
I couldn't find a valid component definition in the following module:

    {module}

To bundle a component, the module should have a public function that returns a
Lustre `App`. Try adding a function like this:

    pub const name: String = \"my-component\"

    pub fn component() -> App(Nil, Model, Msg) {
      lustre.component(init, update, view, on_attribute_change())
    }
"

  message
  |> string.replace("{module}", module)
}

fn network_error(error: Dynamic) -> String {
  let message =
    "
I ran into an unexpected network error while trying to do something. Here's the
error message I got:

    {error}

Please check your internet connection and try again. If you think this is a bug,
please open an issue at https://github.com/lustre-labs/dev-tools/issues/new with
some details about what you were trying to do when you ran into this issue.
"

  message
  |> string.replace("{error}", string.inspect(error))
}

fn template_missing(name: String, reason: simplifile.FileError) -> String {
  let message =
    "
I ran into an unexpected error trying to read an internal template file. This
should never happen! The template file I was looking for is:

    {name}

The error message I got was:

    {reason}

Please open an issue at https://github.com/lustre-labs/dev-tools/issues/new with
the above information and some details about what you were trying to do when you
ran into this issue.
}
"

  message
  |> string.replace("{name}", name)
  |> string.replace("{reason}", string.inspect(reason))
}

fn unknown_file_error(error: simplifile.FileError) -> String {
  let message =
    "
I ran into an unexpected file system error while trying to do something. Here's
the error message I got:

    {error}

If you think this is a bug, please open an issue at
https://github.com/lustre-labs/dev-tools/issues/new with some details about what
you were trying to do when you ran into this issue.
"

  message
  |> string.replace("{error}", string.inspect(error))
}

fn unknown_platform(binary: String, os: String, cpu: String) -> String {
  let path = "./build/.lustre/bin/" <> binary
  let message =
    "
I ran into a problem trying to download the {binary} binary. I couldn't find a
compatible binary for the following platform:

    OS: {os}
    CPU: {cpu}

You may be able to build the binary from source and place it at the following
path:

    {path}

If you think this is a bug, please open an issue at
https://github.com/lustre-labs/dev-tools/issues/new with some details about what
you were trying to do when you ran into this issue.
"

  message
  |> string.replace("{binary}", binary)
  |> string.replace("{os}", os)
  |> string.replace("{cpu}", cpu)
  |> string.replace("{path}", path)
}

fn otp_too_old(version: Int) -> String {
  let message =
    "
It looks like you're running an OTP version that is not supported by the dev
tools: {version}.

You should upgrade to OTP 26 or newer to run this command:
https://gleam.run/getting-started/installing/#installing-erlang
"

  message
  |> string.replace("{version}", int.to_string(version))
}

fn unzip_error(error: Dynamic) -> String {
  let message =
    "
I ran into an unexpected error while trying to unzip a file. Here's the error
message I got:

    {error}

If you think this is a bug, please open an issue at
https://github.com/lustre-labs/dev-tools/issues/new with some details about what
you were trying to do when you ran into this issue.
"

  message
  |> string.replace("{error}", string.inspect(error))
}

fn invalid_esbuild_binary() -> String {
  "
It looks like the downloaded Esbuild tarball has a different hash from what I
expected.
"
}

fn invalid_tailwind_binary() -> String {
  "
It looks like the downloaded Tailwind binary has a different hash from what I
expected.
"
}

// UTILS -----------------------------------------------------------------------

fn pretty_type(t: Type) -> String {
  case t {
    Tuple(elements) -> {
      let message = "#({elements})"
      let elements = list.map(elements, pretty_type)

      message
      |> string.replace("{elements}", string.join(elements, ", "))
    }

    Fn(params, return) -> {
      let message = "fn({params}) -> {return}"
      let params = list.map(params, pretty_type)
      let return = pretty_type(return)

      message
      |> string.replace("{params}", string.join(params, ", "))
      |> string.replace("{return}", return)
    }

    Named(name, _package, _module, []) -> name
    Named(name, _package, _module, params) -> {
      let message = "{name}({params})"
      let params = list.map(params, pretty_type)

      message
      |> string.replace("{name}", name)
      |> string.replace("{params}", string.join(params, ", "))
    }

    Variable(id) -> pretty_var(id)
  }
}

fn pretty_var(id: Int) -> String {
  case id >= 26 {
    True -> pretty_var(id / 26 - 1) <> pretty_var(id % 26)

    False -> {
      let id = id + 97
      let assert Ok(var) = bit_array.to_string(<<id:int>>)

      var
    }
  }
}
