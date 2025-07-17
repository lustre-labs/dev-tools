import * as Process from "node:process";
import * as Path from "node:path";
import * as Fs from "node:fs";

// Directories to watch will be paths relative to the root of the Gleam project,
// but that won't necessarily be the current working directory. This walks up from
// the current working directory to find the root of the Gleam project by looking
// for a `gleam.toml` file.
//
const root = (function findGleamRoot(path) {
  const toml = Path.join(path, "gleam.toml");
  return Fs.existsSync(toml) ? path : findGleamRoot(Path.resolve(path, ".."));
})(Process.cwd());

const { name } = await import(Path.join(root, "gleam.toml"));

/**
 * A Bun plugin that resolves imports with `.gleam` extensions to their
 * compiled JavaScript counterparts in the build directory.
 *
 * @type {import('bun').BunPlugin}
 */
export default function BunPluginGleam() {
  return {
    name: "bun-plugin-gleam",

    setup(build) {
      // Handle .gleam file resolution
      build.onResolve({ filter: /\.gleam$/ }, ({ path, importer }) => {
        // Only handle relative imports
        if (!path.startsWith("./") && !path.startsWith("../")) {
          return null;
        }

        // Calculate the absolute path of the imported .gleam file
        const absoluteGleamPath = Path.resolve(Path.dirname(importer), path);

        // Calculate the path relative to the root
        const relativeToSrc = Path.relative(
          Path.join(root, "src"),
          absoluteGleamPath,
        );

        // Remove the .gleam extension
        const withoutExtension = relativeToSrc.replace(/\.gleam$/, "");

        // Build the path to the compiled JavaScript file
        const compiledPath = Path.join(
          root,
          "build",
          "dev",
          "javascript",
          name,
          `${withoutExtension}.mjs`,
        );

        return { path: compiledPath };
      });
    },
  };
}
