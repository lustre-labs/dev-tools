# Base paths

The `tools.lustre.html.base` option should be used if your application is going
to be served under a path other than `/`.

## Referencing static assets

For example, if you have an image at `assets/image/wibble.png` and a base path
configured as `my-project`, you should reference that in your application like
so:

```gleam
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html

pub fn wibble() -> Element(msg) {
  html.img([attribute.src("/my-project/image/wibble.png")])
}
```

Similarly, assets referenced in the `tools.lustre.html.links`,
`tools.lustre.html.scripts`, and `tools.lustre.html.stylesheets` config should
_also_ include the configured base path.

For example to include a stylesheet saved in `assets/style.css`, you would add
the following to your `gleam.toml`:

```toml
[tools.lustre.html]
stylesheets = [
  { href = "/my-project/style.css" }
]
```
