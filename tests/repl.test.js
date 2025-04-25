const child_process = require("node:child_process");
const path = require("node:path");

describe("repl", () => {
    test("1 + 1", (done) => {
        const repl = child_process.spawn("./bin/index.js", ["repl"], {
            cwd: path.join(__dirname, ".."),
            stdio: "pipe"
        });

        repl.stdout.on("data", (data) => {
            if (data.toString() === "> ") {
                repl.stdin.write("1 + 1\n");
            } else if (data.toString() === "\x1B[95m2\x1B[0m\x1B[90m : number\x1B[0m\n") {
                repl.kill();
                done();
            }
        });
    }, 20000);
});