#!/usr/bin/env -S node --stack-size=8192

const fs = require("node:fs");
const child_process = require("node:child_process");
const readline = require("node:readline");
const os = require("node:os");
const http = require("node:http");
const https = require("node:https");
const resolve = require("node:path").resolve;
const zlib = require("node:zlib");
const AdmZip = require("adm-zip");
const which = require("which");
const tmp = require("tmp");
const { Elm } = require("./guida.js");
const FormData = require("form-data");

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
});

let nextCounter = 0;
const ioRefs = {};
const mVars = {};
const lockedFiles = {};
const processes = {};

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

const io = {
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
  dirRemoveFile: function (index, path) {
    fs.unlink(path, (err) => {
      if (err) {
        console.error(err);
      } else {
        this.send({ index, value: null });
      }
    });
  },
  dirRemoveDirectoryRecursive: function (index, path) {
    fs.rm(path, { recursive: true, force: true }, (err) => {
      if (err) {
        console.error(err);
      } else {
        this.send({ index, value: null });
      }
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
  httpFetch: function (index, method, urlStr, headers) {
    const url = new URL(urlStr);
    const client = url.protocol == "https:" ? https : http;
    const req = client.request(url, { method, headers }, (res) => {
      let chunks = [];

      res.on("data", (chunk) => {
        chunks.push(chunk);
      });

      res.on("end", () => {
        const buffer = Buffer.concat(chunks);
        const encoding = res.headers["content-encoding"];

        if (encoding == "gzip") {
          zlib.gunzip(buffer, (err, decoded) => {
            if (err) {
              console.error(err);
            } else {
              this.send({ index, value: decoded && decoded.toString() });
            }
          });
        } else if (encoding == "deflate") {
          zlib.inflate(buffer, (err, decoded) => {
            if (err) {
              console.error(err);
            } else {
              this.send({ index, value: decoded && decoded.toString() });
            }
          });
        } else {
          this.send({ index, value: buffer.toString() });
        }
      });
    });

    req.on("error", (err) => {
      console.error(err);
    });

    req.end();
  },
  httpUpload: function (index, urlStr, headers, parts) {
    const url = new URL(urlStr);
    const client = url.protocol == "https:" ? https : http;

    const form = new FormData();

    parts.forEach((part) => {
      switch (part.type) {
        case "FilePart":
          form.append(part.name, fs.createReadStream(part.filePath));
          break;

        case "JsonPart":
          form.append(part.name, JSON.stringify(part.value), {
            contentType: "application/json",
            filepath: part.filePath,
          });
          break;

        case "StringPart":
          form.append(part.name, part.string);
          break;
      }
    });

    const req = client.request(url, {
      method: "POST",
      headers: { ...headers, ...form.getHeaders() },
    });

    form.pipe(req);

    req.on("response", (res) => {
      res.on("end", () => {
        this.send({ index, value: null });
      });
    });

    req.on("error", (err) => {
      console.error(err);
    });
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
  envGetProgName: function (index) {
    this.send({ index, value: "guida" });
  },
  envGetArgs: function (index) {
    this.send({ index, value: process.argv.slice(2) });
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
  dirWithCurrentDirectory: function (index, path) {
    try {
      process.chdir(path);
      this.send({ index, value: null });
    } catch (err) {
      console.error(`chdir: ${err}`);
    }
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
  dirFindExecutable: function (index, name) {
    this.send({ index, value: which.sync(name, { nothrow: true }) });
  },
  replGetInputLine: function (index, prompt) {
    rl.question(prompt, (answer) => {
      this.send({ index, value: answer });
    });
  },
  replGetInputLineWithInitial: function (index, prompt, left, right) {
    rl.question(prompt + left + right, (answer) => {
      this.send({ index, value: answer });
    });
  },
  procWithCreateProcess: function (index, createProcess) {
    const file = tmp.fileSync();
    const reader = fs.createReadStream(file.name);

    reader.on("open", (fd) => {
      nextCounter += 1;
      processes[nextCounter] = child_process.spawn(
        createProcess.cmdspec.cmd,
        createProcess.cmdspec.args,
        {
          stdio: [
            createProcess.stdin,
            createProcess.stdout,
            createProcess.stderr,
          ],
        }
      );

      this.send({ index, value: { stdin: file.fd, ph: nextCounter } });
    });

    reader.on("data", (chunk) => {
      processes[nextCounter].stdin.end(chunk);
    });
  },
  procWaitForProcess: function (index, ph) {
    processes[ph].on("exit", (code) => {
      this.send({ index, value: code });
    });
  },
  hClose: function (index, fd) {
    fs.close(fd);
    this.send({ index, value: null });
  },
  hFileSize: function (index, fd) {
    const stats = fs.fstatSync(fd);
    this.send({ index, value: stats.size });
  },
  withFile: function (index, filename, mode) {
    var fd = fs.openSync(filename, mode);
    this.send({ index, value: fd });
  },
};

const app = Elm.Terminal.Main.init();

app.ports.send.subscribe(function ({ index, value }) {
  const fn = io[value.fn];
  fn.apply(app.ports.recv, [index, ...value.args]);
});
