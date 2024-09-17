#!/usr/bin/env node

const fs = require("fs");
const path = require("path");
const childProcess = require("child_process");
const os = require("os");
const tmpDir = os.tmpdir();
const Handlebars = require("handlebars");

var jsFile = `${tmpDir}/guida-cli-${process.pid}.js`;

try {
  childProcess.execSync(
    `elm make terminal/src/Main.elm --debug --output ${jsFile}`
  );
} catch (e) {
  console.error(e);
  process.exit(255);
}

const templatePath = path.join(__dirname, "program.hbs.js");
const templateSource = fs.readFileSync(templatePath).toString();
const template = Handlebars.compile(templateSource);
const sourceCode = template({
  elm: new Handlebars.SafeString("\n" + fs.readFileSync(jsFile).toString()),
});

fs.writeFileSync("./bin/guida.js", sourceCode, { mode: 0o755 });
fs.unlinkSync(jsFile);
