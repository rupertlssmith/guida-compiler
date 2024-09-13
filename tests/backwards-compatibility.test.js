const fs = require("fs");
const path = require("path");
const childProcess = require("child_process");
const os = require("os");
const tmpDir = os.tmpdir();

const examples = ["Hello", "Buttons", "Clock", "HttpQuotes", "Cards"];
const flags = ["no-flags", "debug", "optimize"];

const generateFlags = function (flag) {
  if (flag === "no-flags") {
    return "";
  } else {
    return `--${flag}`;
  }
};

describe("backwards compatibility", () => {
  describe.each(examples)(
    "produces the same code as elm for the %s example",
    (example) => {
      test.each(flags)("%s", (flagOpt) => {
        const elmOutput = `${tmpDir}/guida-test-elm-${example}-${flagOpt}-${process.pid}.js`;
        const guidaOutput = `${tmpDir}/guida-test-guida-${example}-${flagOpt}-${process.pid}.js`;
        const flag = generateFlags(flagOpt);

        try {
          childProcess.execSync(
            `elm make ./src/${example}.elm ${flag} --output ${elmOutput}`,
            { cwd: path.join(__dirname, "..", "examples") }
          );
        } catch (e) {
          console.error(e);
        }

        try {
          childProcess.execSync(
            `../bin/index.js make ./src/${example}.elm ${flag} --output ${guidaOutput}`,
            { cwd: path.join(__dirname, "..", "examples") }
          );
        } catch (e) {
          console.error(e);
        }

        expect(fs.readFileSync(elmOutput).toString()).toBe(
          fs.readFileSync(guidaOutput).toString()
        );
      });
    }
  );
});
