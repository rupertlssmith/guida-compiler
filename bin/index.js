#!/usr/bin/env -S node --stack-size=8192

const { Command } = require("commander");
const program = new Command();
const guida = require("./guida.js");

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
