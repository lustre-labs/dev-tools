// IMPORTS ---------------------------------------------------------------------

import argv
import booklet
import filepath
import gleam/erlang/process
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import glint.{type Command}
import justin
import lustre_dev_tools/bin/bun
import lustre_dev_tools/bin/gleam
import lustre_dev_tools/bin/tailwind
import lustre_dev_tools/build/html
import lustre_dev_tools/cli
import lustre_dev_tools/dev/proxy.{type Proxy}
import lustre_dev_tools/dev/server
import lustre_dev_tools/dev/watcher
import lustre_dev_tools/error.{type Error}
import lustre_dev_tools/project.{type Project}
import lustre_dev_tools/system
import simplifile

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let result = {
    use project <- result.try(project.initialise())

    let args = argv.load().arguments
    let cli =
      glint.new()
      |> glint.as_module
      |> glint.with_name("lustre/dev")
      |> glint.pretty_help(glint.default_pretty_help())
      //
      |> glint.add(at: ["add"], do: add(project))
      |> glint.add(at: ["build"], do: build(project))
      // |> glint.add(at: ["eject"], do: todo)
      // |> glint.add(at: ["gen"], do: todo)
      // |> glint.add(at: ["mcp"], do: todo)
      |> glint.add(at: ["start"], do: start(project))
      |> glint.add(at: [], do: start(project))

    case glint.execute(cli, args) {
      Ok(glint.Help(help)) -> Ok(io.println(help))
      Ok(glint.Out(Ok(_))) -> Ok(Nil)
      Ok(glint.Out(Error(reason))) -> Error(reason)
      Error(message) -> Ok(io.println_error(message))
    }
  }

  case result {
    Ok(_) -> Nil
    Error(reason) -> {
      io.println_error(error.explain(reason))
      system.exit(1)
    }
  }
}

// COMMANDS: ADD ---------------------------------------------------------------

type AddOptions {
  AddOptions(integrations: List(String))
}

fn add(project: Project) -> Command(Result(Nil, Error)) {
  use <- glint.command_help({
    "
Add various binary dependencies to your project. Lustre uses various external
tools to provide core functionality such as bundling JavaScript with Bun or
building styles with Tailwind. This command can be used to download these
integrations from GitHub for dev tools to use.


Supported arguments are:


- `bun`: Bun is a fast JavaScript runtime and bundler. It is used to bundle
your Gleam code into a single JavaScript file that can be run in the browser.


- `tailwind`, `tailwindcss`, or `tw`: Tailwind is a utility-first CSS framework
supported automatically by these dev tools. This command will download the
Tailwind CLI tool.


Lustre will detect which integrations your project needs based on your code and
configuration, and will automatically download necessary tools when you run any
of the other commands. However ou may still want to run this command manually to
ensure that your project has all the necessary tools installed before you go
offline, for example.
    "
  })

  use get_integration <- glint.named_arg("integration")
  use <- glint.unnamed_args(glint.MinArgs(0))
  use arg, args, _ <- glint.command

  let options = AddOptions(integrations: [get_integration(arg), ..args])
  use integration <- list.try_each(options.integrations)

  case integration {
    "bun" -> bun.download(project, quiet: False)
    "tailwind" | "tailwindcss" | "tw" ->
      tailwind.download(project, quiet: False)
    name -> Error(error.UnknownIntegration(name:))
  }
}

// COMMANDS: BUILD -------------------------------------------------------------

type BuildOptions {
  BuildOptions(
    minify: Bool,
    outdir: String,
    entries: List(String),
    skip_html: Bool,
    skip_tailwind: Bool,
  )
}

fn build(project: Project) -> Command(Result(Nil, Error)) {
  use <- glint.command_help({
    "
Build your Gleam project and produce a JavaScript bundle ready to be served and
run in a Web browser. This command accepts zero or more entry modules as arguments.


- If no entry modules are provided, the module matching the name of your app as
defined in your `gleam.toml` will be used as the entry and the `main` function
in that module will be called when the JavaScript bundle is run. An `index.html`
file will also be generated and contain a script tag to load the produced
bundle.


- If one argument is provided, it should be the name of a module in your project
like `your_app` or `your_app/some_module`. The `main` function in that module
will be called when the JavaScript bundle is run. An `index.html` file will also
be generated and contain a script tag to load the produced bundle.


- If multiple arguments are provided, each should be the name of a module in your
project. Multiple JavaScript bundles will be produced, one for each entry module,
and an additional bundle containing all code shared between every entry module.
In this case no `index.html` file will be generated automatically, and must be
provided manually if needed.


The produced JavaScript bundle(s) will be minified and written to your project's
`priv/static` directory by default. Some optimisations such as dead-code elimination
may also be performed.
    "
  })

  use minify <- cli.bool("minify", ["build", "minify"], project, {
    "
Produce a production-ready minified build of the project. This will rename
variables, remove white space, and perform other optimisations to reduce the
size of the JavaScript output.


This option can also be provided in your `gleam.toml` configuration under the
key `tools.lustre.build.minify`.
    "
  })

  use skip_html <- cli.bool("no-html", ["build", "no_html"], project, {
    "
Skip automatic generation of an HTML file for this project. You might want to
do this if you have a custom HTML file you want to use instead. HTML generation
is always skipped if there are multiple entry modules.


This option can also be provided in your `gleam.toml` configuration under the
key `tools.lustre.build.no_html`.
    "
  })

  use skip_tailwind <- cli.bool(
    "no-tailwind",
    ["build", "no_tailwind"],
    project,
    "
Skip automatic detection of Tailwind CSS in this project. This means even if a
valid Tailwind entry point is detected, Lustre will not attempt to download or
run the Tailwind CLI tool during the build process. You might want to do this if
you have a custom CSS build process you want to use instead.

This option can also be provided in your `gleam.toml` configuration under the
key `tools.lustre.build.no_tailwind`.
    ",
  )

  use outdir <- cli.string("outdir", ["build", "outdir"], project, {
    "
Configure where the build JavaScript bundle will be written to: by default this
is `priv/static` within the project root. Common alternatives include `docs/` for
GitHub Pages sites, `public/`, for hosting with services like Vercel, or the
`priv/static` directory of another Gleam or Elixir project.


This option can also be provided in your `gleam.toml` configuration under the
key `tools.lustre.build.outdir`.
    "
  })

  use <- glint.unnamed_args(glint.MinArgs(0))
  use _, entries, flags <- glint.command
  // If the user did not provide any explicit entry modules, we'll take the
  // app's main module as the entry.
  let entries = case entries {
    [] -> [project.name]
    _ -> entries
  }

  let options =
    BuildOptions(
      minify: minify(flags) |> result.unwrap(False),
      outdir: filepath.join(
        project.root,
        outdir(flags) |> result.unwrap("priv/static"),
      ),
      entries:,
      skip_html: case skip_html(flags), entries {
        Ok(True), _ -> True
        Ok(False), [_, _, ..] -> True
        Ok(False), _ | Error(_), _ -> False
      },
      skip_tailwind: skip_tailwind(flags) |> result.unwrap(False),
    )

  // 1.
  use _ <- result.try(
    simplifile.create_directory_all(options.outdir)
    |> result.map_error(error.CouldNotWriteFile(options.outdir, _)),
  )

  // 2.
  use _ <- result.try(gleam.build(project))

  use bun_entries <- result.try({
    use entry <- list.try_map(options.entries)
    let module =
      "import { main } from '../../build/dev/javascript/${name}/${entry}.mjs'; main();"
      |> string.replace("${name}", project.name)
      |> string.replace("${entry}", entry)

    let name = justin.snake_case(entry) <> ".mjs"
    let path = filepath.join(project.build, name)

    use _ <- result.try(
      simplifile.write(path, module)
      |> result.map_error(error.CouldNotWriteFile(path, _)),
    )

    Ok(path)
  })

  use _ <- result.try(bun.build(
    project,
    entries: bun_entries,
    outdir: options.outdir,
    minify: options.minify,
    quiet: False,
  ))

  // 3.
  let tailwind_entry = case options.entries {
    [entry] -> entry
    [] | [_, ..] -> project.name
  }

  use tailwind_entry <- result.try(
    case tailwind.detect(project, tailwind_entry) {
      Ok(tailwind.HasTailwindEntry) if options.skip_tailwind -> Ok(None)

      Ok(tailwind.HasTailwindEntry) -> {
        use _ <- result.try(tailwind.build(
          project,
          tailwind_entry,
          options.outdir,
          options.minify,
          quiet: False,
        ))

        Ok(Some(tailwind_entry))
      }

      Ok(tailwind.HasViableEntry)
      | Ok(tailwind.Nothing)
      | Ok(tailwind.HasLegacyConfig) -> Ok(None)

      Error(e) -> Error(e)
    },
  )

  // 4.
  use _ <- result.try(case options.entries, options.skip_html {
    _, True | [_, _, ..], _ -> Ok(Nil)

    [], False -> {
      cli.log("Building index.html", False)

      use _ <- result.try(
        html.generate(project, project.name, tailwind_entry)
        |> simplifile.write(filepath.join(options.outdir, "index.html"), _)
        |> result.map_error(error.CouldNotWriteFile(
          filepath.join(options.outdir, "index.html"),
          _,
        )),
      )

      cli.success("HTML generated.", False)

      Ok(Nil)
    }

    [entry], False -> {
      cli.log("Building index.html", False)

      use _ <- result.try(
        html.generate(project, entry, tailwind_entry)
        |> simplifile.write(filepath.join(options.outdir, "index.html"), _)
        |> result.map_error(error.CouldNotWriteFile(
          filepath.join(options.outdir, "index.html"),
          _,
        )),
      )

      cli.success("HTML generated.", False)

      Ok(Nil)
    }
  })

  cli.success("Build complete!", False)

  Ok(Nil)
}

// COMMANDS: START -------------------------------------------------------------

type StartOptions {
  StartOptions(
    watch: List(String),
    proxy: Proxy,
    entry: String,
    tailwind_entry: Option(String),
    host: String,
    port: Int,
  )
}

fn start(project: Project) -> Command(Result(Nil, Error)) {
  use <- glint.command_help({
    "
Start a development server to run your Lustre app locally. This will watch your
source files for changes and automatically rebuild and reload the app in your
browser.
    "
  })

  use host <- cli.string("host", ["dev", "host"], project, {
    "
Configure the host address the development server will listen on: by default this
is `localhost`. You can set this to `0.0.0.0` to allow access from other devices
on yoru local network. This can be useful for testing with real mobile devices.
    "
  })

  use port <- cli.int("port", ["dev", "port"], project, {
    "
Configure the port the development server will listen on: by default this is
`1234`. If this port is already in use the server will fail to start.
    "
  })

  use proxy_from <- cli.string("", ["dev", "proxy", "from"], project, "")
  use proxy_to <- cli.string("", ["dev", "proxy", "to"], project, "")

  use watch <- cli.string_list("watch", ["dev", "watch"], project, {
    "
Configure additional directories to watch for changes. The `src/` and `assets/`
directories are always watched and do not need to be specified here.
    "
  })

  use <- glint.unnamed_args(glint.MinArgs(0))
  use _, entries, flags <- glint.command

  use proxy <- result.try(proxy.new(
    proxy_from(flags) |> result.unwrap(""),
    proxy_to(flags) |> result.unwrap(""),
  ))

  let entry = case entries {
    [] -> project.name
    [entry, ..] -> entry
  }

  use tailwind_entry <- result.try(case tailwind.detect(project, entry) {
    Ok(tailwind.HasTailwindEntry) -> Ok(Some(entry))
    Ok(tailwind.HasViableEntry)
    | Ok(tailwind.Nothing)
    | Ok(tailwind.HasLegacyConfig) -> Ok(None)
    Error(e) -> Error(e)
  })

  let options =
    StartOptions(
      watch: watch(flags)
        |> result.unwrap([])
        |> list.filter(fn(dir) {
          case simplifile.is_directory(dir) {
            Ok(True) -> True
            Ok(False) | Error(_) -> False
          }
        })
        |> list.append([project.src, project.assets]),
      proxy:,
      entry:,
      tailwind_entry:,
      host: host(flags) |> result.unwrap("localhost"),
      port: port(flags) |> result.unwrap(1234),
    )

  use _ <- result.try(gleam.build(project))
  use _ <- result.try(case options.tailwind_entry {
    Some(tailwind_entry) ->
      tailwind.build(
        project,
        tailwind_entry,
        filepath.join(project.root, "build/dev/javascript"),
        False,
        quiet: False,
      )
    None -> Ok(Nil)
  })

  let error = booklet.new(None)

  // Start the file watcher and set up a process registry so connected dev server
  // clients can be notified when files change. This should use Bun and the file
  // watcher script in `priv/bun-watcher.js` but if that fails to start it can
  // fall back to file system polling.
  let watcher =
    watcher.start(project, error, options.watch, options.tailwind_entry)

  use _ <- result.try(server.start(
    project,
    error,
    watcher,
    options.proxy,
    options.entry,
    options.tailwind_entry,
    options.host,
    options.port,
  ))

  Ok(process.sleep_forever())
}
