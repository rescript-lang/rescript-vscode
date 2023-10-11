#!/usr/bin/env node
// @ts-check
const fs = require("fs");
const path = require("path");
const { spawnSync } = require("child_process")

const args = process.argv.slice(2)

const platformDir = process.arch === "arm64" ? process.platform + process.arch : process.platform;

const analysisProdPath = path.join(
  path.dirname(__dirname),
  "analysis_binaries",
  platformDir,
  "rescript-editor-analysis.exe"
);

const doc_help = `ReScript Tools

Output documentation to standard output

Usage: restools doc <FILE>

Example: restools doc ./path/to/EntryPointLib.res`;

const help = `ReScript Tools

Usage: restools [command]

Commands:

doc                   Generate documentation
reanalyze             Reanalyze
-v, --version         Print version
-h, --help            Print help`;


if (args[0] == "doc") {

  if (args[1] == undefined) {
    console.log(doc_help)
    process.exit(1)
  }
  if (args[1] == "--help" || args[1] == "-h") {
    console.log(doc_help)
    process.exit(0)
  }

  const spawn = spawnSync(analysisProdPath, ["extractDocs", args[1]]);

  if (spawn.status !== 0) {
    console.log(spawn.stderr.toString().trim())
    process.exit(spawn.status || 1)
  }

  console.log(spawn.stdout.toString());
  process.exit(spawn.status);

} else if (args[0] == "reanalyze") {
  const restArgs = args.slice(1)

  const spawn = spawnSync(analysisProdPath, ["reanalyze", ...restArgs]);

  if (spawn.status !== 0) {
    console.log(spawn.stderr.toString().trim())
    process.exit(spawn.status || 1)
  }

  console.log(spawn.stdout.toString());
  process.exit(spawn.status);

} else if (args[0] == "--help" || args[0] == "-h") {
  console.log(help);
  process.exit(0)
} else if (args[0] == "--version" || args[0] == "-v") {
  console.log(JSON.parse(fs.readFileSync('./package.json', { encoding: 'utf8' })).version);
  process.exit(0);
} else {
  console.log(help);
  process.exit(1)
}
