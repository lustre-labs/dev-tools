import * as Process from "node:process";
import * as Path from "node:path";
import * as Fs from "node:fs";
import * as Readline from "node:readline";

// Directories to watch will be paths relative to the root of the Gleam project,
// but that won't necessarily be the current working directory. This walks up from
// the current working directory to find the root of the Gleam project by looking
// for a `gleam.toml` file.
//
const root = (function findGleamRoot(path) {
  const toml = Path.join(path, "gleam.toml");
  return Fs.existsSync(toml) ? path : findGleamRoot(Path.resolve(path, ".."));
})(Process.cwd());

// Each directory to watch is stored in a `Map`
const watchers = new Map();
const options = { recursive: true, persistent: true };
const onChange =
  (dir) =>
  (type, filename = dir) => {
    console.log(JSON.stringify({ $: type, in: dir, name: filename }));
  };

//
const stdin = Readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false,
});

//
stdin.on("line", (line) => {
  const data = JSON.parse(line.trim() || "{}");

  switch (data.$) {
    case "start": {
      if (watchers.has(data.path)) return;

      try {
        watchers.set(
          data.path,
          Fs.watch(Path.join(root, data.path), options, onChange(data.path)),
        );
      } finally {
        return;
      }
    }

    case "stop": {
      const watcher = watchers.get(data.path);

      if (watcher) {
        watcher.close();
        watchers.delete(data.path);
      }

      return;
    }
  }
});

//
stdin.on("close", () => {
  for (const [key, watcher] of watchers) {
    watcher.close();
    watchers.delete(key);
  }

  process.exit(0);
});

process.on("SIGINT", () => {
  for (const [key, watcher] of watchers) {
    watcher.close();
    watchers.delete(key);
  }

  stdin.close();
});

process.on("SIGTERM", () => {
  for (const [key, watcher] of watchers) {
    watcher.close();
    watchers.delete(key);
  }

  stdin.close();
});
