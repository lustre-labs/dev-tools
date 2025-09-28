# TOML reference

Lustre's dev tools follow Gleam's tools convention and can be configured in your
project's `gleam.toml` file under the `tools.lustre` table.

```toml
[tools.lustre.bin]
...

[tools.lustre.build]
...

[tools.lustre.dev]
...

[tools.lustre.html]
...
```

> **Note**: that any flags passed to the command line will always take precedence
> over any configuration in your `gleam.toml`

## `tools.lustre.bin`

These options allow you to configure what binaries Lustre uses as part of the
build and development process.

- **`bun = "system" | string`**: choose a local Bun binary to use instead of letting
  Lustre download and manage its own Bun version. You can specify a path to the
  Bun binary you want to use, or the string `"system"` to look up the `bun`
  executable in your system `PATH`.

  Default: `undefined`. Lustre will download and manage its own Bun version.

- **`tailwind = "system" | string`**: choose a local Tailwind CSS binary to use
  instead of letting Lustre download and manage its own Tailwind version. You
  can specify a path to the Tailwind binary you want to use, or the string
  `"system"` to look up the `tailwindcss` in your system `PATH`.

  Default: `undefined`. Lustre will download and manage its own Tailwind version.

## `tools.lustre.build`

These options affect how Lustre builds your project when you run
`gleam run -m lustre/dev build`.

- **`minify = true | false`**: whether or not to minify the output JavaScript and
  CSS bundles. A minified bundle renames variables, removes whitespace, and can
  perform other optimisations to reduce the size of the output files.

  Default: `false`.

- **`no-html = true | false`**: whether or not to skip generating a HTML entry file
  for the output bundle. If set to `true`, Lustre will only generate JavaScript
  and CSS bundles and you will need to write or generate your own HTML document
  separately.

  If multiple entry points are specified, this option is ignored and a HTML file
  will never be generated.

  Default: `false`

- **`outdir = string`**: choose the output directory that any built files should be
  written to. This should be a path relative to the project root (where `gleam.toml`
  is located).

  Common choices include `./dist`, `./docs`, `./public`, or another Gleam project's
  `priv/static` directory.

  Default: `./priv/static`.

## `tools.lustre.dev`

These options affect the development server that runs when you run
`gleam run -m lustre/dev start`.

- **`host = "localhost" | "0.0.0.0"`**: choose which network interface the development
  server should bind to. Use `"localhost"` to only allow connections from the
  local machine, or `"0.0.0.0"` to allow connections from other devices on the
  same network.

  Default: `"localhost"`.

- **`port = number`**: choose which port the development server should listen on. If
  the specified port is already in use, the development server will fail to
  start.

  Default: `1234`.

- **`proxy = { from: string, to: string }`**: configure an API proxy to forward
  requests from the app to a different server. This lets you avoid CORS issues
  while the frontend and backend are running on different ports during development.

  The `from` field is a path prefix to match request against like `/api` and the
  `to` field should be a full valid URI like `http://localhost:3000`. Any path
  after the `from` prefix will be appended to the `to` URI when forwarding the
  request.

  For example, with the above configuration a request to `/api/users` would be
  forwarded to `http://localhost:3000/users`.

  Default: `undefined`.

- **`watch = string[]`**: an array of directories relative to the project root to watch
  for changes. When a change is detected in one of these directories, the
  development server will automatically reload the page in the browser.

  The `./src` and `./assets` directories are always watched and do not need to
  be specified here.

  Default: `[]`.

## `tools.lustre.html`

These options control the HTML document that Lustre generates when building your
application or serves during development.

- **`body = string`**: any custom HTML content to include inside the `<body>` tag
  of the generated HTML document. This can be used to add static content to the
  page or to change the default root element that is generated.

  Default: `<div id="app"></div>`.

- **`lang = string`**: the language attribute to set on the `<html>` tag of the
  generated HTML document. This should be a valid
  [BCP 47 language tag](https://www.rfc-editor.org/rfc/bcp/bcp47.txt) like `en`
  for English or `fr` for French.

  Default: `en`.

- **`meta = { [key: string]: string }[]`**: an array of `<meta>` tags to include
  in the `<head>` of the generated HTML document. Each object in the array
  represents a single `<meta>` tag where the keys are the attribute names and
  the values are the attribute values.

  Two meta tags are always included: `<meta charset="UTF-8">` and
  `<meta name="viewport" content="width=device-width, initial-scale=1.0">`. You
  do not need to include these yourself.

  Default: `[]`.

- **`scripts = ({ src: string, type?: string } | { content: string, type?: string })[]`**:
  an array of `<script>` tags to include at the end of the `<body>` in the
  generated HTML document. Each object in the array can either represent an
  external script with a `src` attribute or an inline script with a `content`
  attribute. The optional `type` attribute can be used to specify the script
  type, such as `module` for ES modules.

  Lustre will always generate the script tag for the main JavaScript bundle after
  any scripts specified here.

  Default: `[]`.

- **`stylesheets = ({ href: string} | { content: string })[]`**: an array of
  stylesheets to include in the `<head>` of the generated HTML document. Each
  object in the array can either represent an external stylesheet with an `href`
  attribute or an inline stylesheet with a `content` attribute.

  Lustre will always generate the link tag for the the built Tailwind CSS bundle
  after any stylesheets specified here.

  Default: `[]`.

- **`title = string`**: choose the title of the generated HTML document. This will
  be included in the `<title>` tag in the `<head>` of the document.

  Default: the name of the Gleam project as specified in `gleam.toml`.
