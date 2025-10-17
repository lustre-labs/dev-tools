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

#### Loading custom CSS
For example if you want to load your custom css file named `style.css` place it in the `assets` folder
in your root project dir and edit the gleam.toml with

```
[tools.lustre.html]
stylesheets = [{ href = "/style.css" }]
```

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
