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
    <a href="#commands">
      Commands
    </a>
    <span> | </span>
    <a href="https://discord.gg/Fm8Pwmy">
      Discord
    </a>
  </h3>
</div>

<div align="center">
  <sub>Built with ❤︎ by
  <a href="https://twitter.com/hayleighdotdev">Hayleigh Thompson</a> and
  <a href="https://twitter.com/giacomo_cava">Giacomo Cavalieri</a>
</div>

---

## Features

- Built-in **development server**.

- Automatic detection and support for **TailwindCSS**.

- **Bundle** and **minify** Lustre applications.

- Distribute reusable **Web Components** for use outside of Gleam and Lustre.

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
You can add them to your Gleam projects from the command line:

```sh
gleam add lustre_dev_tools --dev
```

> **Note**: make sure you remember the `--dev` flag! This ensures the dev tools
> are never included in production builds.

To run any of the commands provided by the dev tools, you should run the
`lustre/dev` module using Gleam's `run` command:

```sh
gleam run -m lustre/dev build app
```

## Commands

- `lustre/dev add` - Commands for adding external binaries to your project. These
  are run and managed by Lustre, and while not typically intended to be run manually,
  they can be found inside `build/.lustre/bin`.

  - `lustre/dev add esbuild` - Download a platform-appropriate version of the
    esbuild binary. Lustre uses this to bundle applications and act as a development
    server, and will automatically download the binary if either the `build` or
    `start` commands are run.

  - `lustre/dev add tailwind` - Download a platform-appropriate version of the
    Tailwind binary. Lustre will automatically use this to compile your styles if
    it detects a `tailwind.config.js` in your project but will not download it
    automatically.

- `lustre/dev build` - Commands to build different kinds of Lustre application.
  These commands go beyond just running `gleam build` and handle features like
  bundling, minification, and integration with other build tools.

  - `lustre/dev build app` - Build and bundle an entire Lustre application. The
    generated JavaScript module calls your app's `main` function on page load and
    can be included in any Web page without Gleam or Lustre being present.

  - `lustre/dev build component` - Build a Lustre component as a portable Web
    Component. The generated JavaScript module can be included in any Web page
    and used without Gleam or Lustre being present.

- `lustre/dev start` - Start a development server for your Lustre project. This
  command will compile your application and serve it on a local server. If your
  application's `main` function returns a compatible `App`, this will generate
  the necessary code to start it. Otherwise, your `main` function will be used
  as the entry point.

## Support

Lustre is mostly built by just me, [Hayleigh](https://github.com/hayleigh-dot-dev),
around two jobs. If you'd like to support my work, you can [sponsor me on GitHub](https://github.com/sponsors/hayleigh-dot-dev).

Contributions are also very welcome! If you've spotted a bug, or would like to
suggest a feature, please open an issue or a pull request.
