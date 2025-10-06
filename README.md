<h1 align="center">Lustre Dev Tools</h1>

<div align="center">
  Lustre's official CLI and development tooling.
</div>

<br />

<div align="center">
  <a href="https://hex.pm/packages/lustre_dev_tools">
  <img src="https://img.shields.io/hexpm/v/lustre_dev_tools"
      alt="Available on Hex" />
  </a>
</div>

<div align="center">
  <h3>
    <a href="https://hexdocs.pm/lustre">
      Lustre
    </a>
    <span> | </span>
    <a href="https://hexdocs.pm/lustre_dev_tools/toml-reference.html">
        TOML reference
    </a>
    <span> | </span>
    <a href="https://discord.gg/Fm8Pwmy">
      Discord
    </a>
  </h3>
</div>

<div align="center">
  <sub>Built with ❤︎ by
  <a href="https://twitter.com/hayleighdotdev">Hayleigh Thompson</a>
</div>

---

## Features

- Built-in **development server** with live reloading.

- Automatic detection and support for **TailwindCSS v4**.

- **Bundle** and **minify** Lustre applications.

## Philosophy

Lustre's dev tools are designed to make the experience as simple as possible for
folks unfamiliar with frontend development or are exhausted at the state of
JavaScript tooling. That means being opinionated about the tools we support and
the commands we provide.

If you find yourself needing more configuration or control over your build process,
you might be outgrowing what Lustre's dev tools have set out to provide! We'd love
to hear from those users too, though, so please open an issue or reach out on Discord
if you think something is missing.

For more advanced users, we recommend using [vite](https://vitejs.dev) and the
[vite-gleam](https://github.com/Enderchief/gleam-tools/tree/master/packages/vite-gleam)
package.

## Installation

Lustre's dev tools are published on [Hex](https://hex.pm/packages/lustre_dev_tools)!
You can add them as a dev dependency to your Gleam projects from the command line:

```sh
gleam add lustre_dev_tools --dev
```

The `--dev` flag is important to make sure the dev tools are not included in your
application's build!

To run any of the commands provided by the dev tools, you should run the
`lustre/dev` module using Gleam's `run` command:

```sh
gleam run -m lustre/dev start
```

## Static assets

Lustre's dev tools uses a top-level `assets` directory as a place to store static
assets like images, fonts, or other scripts and stylesheets that should not be
part of the build process. You can read more about how to use the `assets` directory
in the [assets documentation](https://hexdocs.pm/lustre_dev_tools/assets.html).

## External binaries

Lustre's dev tools are a wrapper around existing binary build tools. Specifically,
we use [bun](https://bun.sh/) for bundling and file watching and we also support Tailwind
through their standalone CLI.

Lustre will automatically detect your platform and download the appropriate
binaries when you first run a command. These binaries are cached _per-project_
in the project's `.lustre` directory. If you prefer, Lustre can be told to use a
local path or check your system's `$PATH` for the binaries instead. 
See the [TOML reference](https://hexdocs.pm/lustre_dev_tools/toml-reference.html)
for an overview on how to define those options.

## Support

Lustre is mostly built by just me, [Hayleigh](https://github.com/hayleigh-dot-dev),
around two jobs. If you'd like to support my work, you can [sponsor me on GitHub](https://github.com/sponsors/hayleigh-dot-dev).

Contributions are also very welcome! If you've spotted a bug, or would like to
suggest a feature, please open an issue or a pull request.
