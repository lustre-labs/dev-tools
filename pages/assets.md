# Handling assets

Many Web apps will also want to include static assets like images, fonts, or
code that should be excluded from the build process like static CSS files or
additional scripts.

Lustre supports assets placed in a top-level `assets` directory. During development,
any files in this directory will be served by the development server and can be
referenced in your application using the asset's path starting from the `assets`
directory.

For example, if you have an image at `assets/image/wibble.png`, you might reference
that in your application like so:

```gleam
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub fn wibble() -> Element(msg) {
  html.img([attribute.src("/image/wibble.png")])
}
```


> **Note**: note how the image's path is `/image/wibble.png` and not
> `/assets/image/wibble.png`!

When you build your application, the contents of the `assets` directory will be
copied to the output directory specified in your `gleam.toml` file. So if we build
the above example, the build artifacts might look something like:

```
dist/
├── index.html
├── app.js
└── image/
    └── wibble.png
```

## Static CSS

The most common use of the `assets` directory is to include static CSS instead of
using the built-in Tailwind integration. To do this you'll also need to teach
Lustre how to load your CSS into the generated HTML by adding some configuration
to your `gleam.toml` file.

For example, let's say you've written some CSS and saved it to `assets/style.css`.
To make sure the stylesheet is properly loaded both during development and in the
built output, you would add the following to your `gleam.toml` file:

```
[tools.lustre.html]
stylesheets = [
  { href = "/style.css" }
]
```

Lustre has special hot-relaoding support for CSS files included this way, so any
changes you make will be reflected in the browser without needing to refresh the
page!

## External build tools

The `assets` directory is also a suitable place to emit files from additional
build tools for Lustre to include in the final output. For example, you may
prefer to write your CSS in a language like Sass or Less and run a separate build
process to compile those files to CSS.

You would configure your build tool to output the compiled CSS files to the `assets`
directory, and make sure to run that build process _before_ building your Lustre
app or starting the development server.

> **Note**: if you find yourself starting to integrate multiple build tools to
> handle different parts of your asset pipeline, you may want to consider using
> a more fully-featured build tool like [Vite](https://vitejs.dev/) or
> [Webpack](https://webpack.js.org/) instead of Lustre's built-in development
> tooling.
