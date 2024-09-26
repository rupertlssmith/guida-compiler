#!/usr/bin/env -S node --stack-size=8192

const fs = require("fs");
const readline = require("readline");
const os = require("os");
const https = require("https");
const resolve = require("path").resolve;
const AdmZip = require("adm-zip");

const { Command } = require("commander");
const program = new Command();
const { Elm } = require("./guida.js");

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
});

let nextCounter = 0;
const ioRefs = {};
const mVars = {};
const lockedFiles = {};

const download = function (index, method, url) {
  const req = https.request(url, { method: method }, (res) => {
    if (res.statusCode >= 200 && res.statusCode < 300) {
      let data = [];
      res.on("data", (chunk) => {
        data.push(chunk);
      });

      res.on("end", () => {
        const zip = new AdmZip(Buffer.concat(data));

        const jsonData = zip.getEntries().map(function (entry) {
          return {
            eRelativePath: entry.entryName,
            eData: zip.readAsText(entry),
          };
        });

        this.send({ index, value: jsonData });
      });
    } else if (res.headers.location) {
      download.apply(this, [index, method, res.headers.location]);
    }
  });

  req.on("error", (e) => {
    console.error(e);
  });

  req.end();
};

function createIo(rl) {
  return {
    fwrite: function (index, fd, content) {
      this.send({ index, value: fs.writeSync(fd, content) });
    },
    fread: function (index, fd) {
      this.send({ index, value: fs.readFileSync(fd).toString() });
    },
    fopen: function (index, filename, flags) {
      try {
        this.send({ index, value: fs.openSync(filename, flags) });
      } catch (e) {
        this.send({ index, value: e.toString() });
      }
    },
    mkdir: function (index, name, recursive) {
      fs.mkdirSync(name, { recursive: recursive, mode: 0o777 });
      this.send({ index, value: null });
    },
    fstat: function (index, filename) {
      try {
        this.send({ index, value: fs.statSync(filename) });
      } catch (e) {
        this.send({ index, value: e.toString() });
      }
    },
    readdir: function (index, dirname) {
      try {
        var r = fs
          .readdirSync(dirname, { withFileTypes: true })
          .map((dirent) => ({
            name: dirent.name,
            isDir: dirent.isDirectory(),
            isFile: dirent.isFile(),
            isSocket: dirent.isSocket(),
            isFifo: dirent.isFIFO(),
            isSymlink: dirent.isSymbolicLink(),
            isBlockDevice: dirent.isBlockDevice(),
            isCharacterDevice: dirent.isCharacterDevice(),
          }));
        this.send({ index, value: r });
      } catch (e) {
        this.send({ index, value: e.toString() });
      }
    },
    exit: function (_index, _errorMessage, status) {
      rl.close();
      process.exit(status);
    },
    dirDoesFileExist: function (index, filename) {
      this.send({ index, value: fs.existsSync(filename) });
    },
    getLine: function (index) {
      rl.on("line", (input) => {
        this.send({ index, value: input });
      });
    },
    newIORef: function (index, value) {
      nextCounter += 1;
      ioRefs[nextCounter] = { subscribers: [], value };
      this.send({ index, value: nextCounter });
    },
    readIORef: function (index, id) {
      if (ioRefs[id].value === null) {
        ioRefs[id].subscribers.push(index);
      } else {
        this.send({ index, value: ioRefs[id].value });
      }
    },
    dirCreateDirectoryIfMissing: function (index, createParents, filename) {
      fs.mkdir(filename, { recursive: createParents }, (err) => {
        this.send({ index, value: null });
      });
    },
    writeIORef: function (index, id, value) {
      ioRefs[id].value = value;

      const subscribers = ioRefs[id].subscribers;
      ioRefs[id].subscribers = [];
      subscribers.forEach((subscriber) => {
        this.send({ index: subscriber, value });
      });

      this.send({ index, value: null });
    },
    httpFetch: function (index, method, url) {
      const req = https.request(url, { method: method }, (res) => {
        let data = [];
        res.on("data", (chunk) => {
          data.push(chunk);
        });

        res.on("end", () => {
          this.send({ index, value: Buffer.concat(data).toString() });
        });
      });

      req.on("error", (e) => {
        console.error(e);
      });

      req.end();
    },
    write: function (index, path, value) {
      this.send({
        index,
        value: fs.writeFileSync(path, JSON.stringify(value)),
      });
    },
    writeString: function (index, path, str) {
      this.send({ index, value: fs.writeFileSync(path, str) });
    },
    envLookupEnv: function (index, varname) {
      this.send({ index, value: process.env[varname] });
    },
    binaryDecodeFileOrFail: function (index, filename) {
      this.send({
        index,
        value: JSON.parse(fs.readFileSync(filename).toString()),
      });
    },
    dirGetAppUserDataDirectory: function (index, app) {
      this.send({ index, value: os.homedir() + "/." + app });
    },
    dirGetCurrentDirectory: function (index) {
      this.send({ index, value: process.cwd() });
    },
    dirGetModificationTime: function (index, filename) {
      fs.stat(filename, (error, stats) => {
        if (error) {
          console.log(error);
        } else {
          this.send({ index, value: parseInt(stats.mtimeMs, 10) });
        }
      });
    },
    dirDoesDirectoryExist: function (index, path) {
      this.send({ index, value: fs.existsSync(path) });
    },
    dirCanonicalizePath: function (index, path) {
      this.send({ index, value: resolve(path) });
    },
    getArchive: function (index, method, url) {
      download.apply(this, [index, method, url]);
    },
    lockFile: function (index, path) {
      if (lockedFiles[path]) {
        lockedFiles[path].subscribers.push(index);
      } else {
        lockedFiles[path] = { subscribers: [] };
        this.send({ index, value: null });
      }
    },
    unlockFile: function (index, path) {
      if (lockedFiles[path]) {
        const subscriber = lockedFiles[path].subscribers.shift();

        if (subscriber) {
          this.send({ index: subscriber, value: null });
        } else {
          delete lockedFiles[path];
        }

        this.send({ index, value: null });
      } else {
        console.error(`Could not find locked file "${path}"!`);
        rl.close();
        process.exit(255);
      }
    },
    newEmptyMVar: function (index) {
      nextCounter += 1;
      mVars[nextCounter] = { subscribers: [], value: null };
      this.send({ index, value: nextCounter });
    },
    readMVar: function (index, id) {
      if (mVars[id].value === null) {
        mVars[id].subscribers.push({ index, action: "read" });
      } else {
        this.send({ index, value: mVars[id].value });
      }
    },
    takeMVar: function (index, id) {
      if (mVars[id].value === null) {
        mVars[id].subscribers.push({ index, action: "take" });
      } else {
        const value = mVars[id].value;
        mVars[id].value = null;

        if (
          mVars[id].subscribers.length > 0 &&
          mVars[id].subscribers[0].action === "put"
        ) {
          const subscriber = mVars[id].subscribers.shift();
          mVars[id].value = subscriber.value;
          this.send({ index: subscriber.index, value: null });
        }

        this.send({ index, value });
      }
    },
    putMVar: function (index, id, value) {
      if (mVars[id].value === null) {
        mVars[id].value = value;

        mVars[id].subscribers = mVars[id].subscribers.filter((subscriber) => {
          if (subscriber.action === "read") {
            this.send({ index: subscriber.index, value });
          }

          return subscriber.action !== "read";
        });

        const subscriber = mVars[id].subscribers.shift();

        if (subscriber) {
          this.send({ index: subscriber.index, value });

          if (subscriber.action === "take") {
            mVars[id].value = null;
          }
        }

        this.send({ index, value: null });
      } else {
        mVars[id].subscribers.push({ index, action: "put", value });
      }
    },
  };
}

const main = function (flags) {
  const io = createIo(rl);
  const app = Elm.Main.init({ flags: flags });

  app.ports.send.subscribe(function ({ index, value }) {
    const fn = io[value.fn];
    fn.apply(app.ports.recv, [index, ...value.args]);
  });
};

const guida = {
  init: () => {
    main({ command: "init" });
  },
  make: (paths, options = {}) => {
    main({ command: "make", paths, ...options });
  },
  install: (package) => {
    main({ command: "install", package });
  },
  repl: (options = {}) => {
    main({ command: "repl", ...options });
  },
};

const description = `Guida is a functional programming language that builds upon the solid foundation of Elm,
offering backward compatibility with all existing Elm 0.19.1 projects.`;
const version = "0.1.0";

program.name("guida").description(description).version(version);

// init command

const initDescription = `Start an Elm project. It creates a starter elm.json file and provides a
link explaining what to do from there.`;

program
  .command("init")
  .description(initDescription)
  .action(() => {
    guida.init();
  });

// make command

const makeDescription = `The \`make\` command compiles Elm code into JS or HTML.`;

program
  .command("make")
  .description(makeDescription)
  .argument("<elm-files...>")
  .option("--debug")
  .option("--optimize")
  .option("--output <output-file>")
  .option("--report <report-type>")
  .option("--docs <json-file>")
  .action((paths, options) => {
    guida.make(paths, options);
  });

// install command

const installDescription = `The \`install\` command fetches packages from <https://package.elm-lang.org>
for use in your project.`;

program
  .command("install")
  .description(installDescription)
  .argument("[package]")
  .action((package) => {
    guida.install(package);
  });

// repl command

const replDescription = `The \`repl\` command opens up an interactive programming session.`;

program
  .command("repl")
  .description(replDescription)
  .option("--interpreter=<interpreter>")
  .option("--no-colors")
  .action((options) => {
    guida.repl(options);
  });

program.parse();
