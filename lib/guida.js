#!/usr/bin/env node

const childProcess = require("child_process");

try {
  childProcess.execSync(
    `elm make src/Terminal/Main.elm --debug --output ./bin/guida.js`
  );
} catch (e) {
  console.error(e);
  process.exit(255);
}
