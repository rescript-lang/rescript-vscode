#!/usr/bin/env node
import fs from "fs";
import path from "path";
import server from "./server";

const args = process.argv.slice(2)

const help = `ReScript Language Server

Usage: rescript-language-server [options]

Options:

--stdio               Use stdio
--node-ipc            Use node-ipc
-v, --version         Print version
-h, --help            Print help`;

(() => {
  switch (args[0]) {
    case '--stdio':
      return server(true);
    case '--node-ipc':
      return server(false);
    case '--version':
    case '-v':
      const { version } = JSON.parse(fs.readFileSync(path.join(__dirname, "..", "package.json")).toString())
      console.log(version);
      process.exit(0);
    case '--help':
    case '-h':
      console.log(help);
      process.exit(0);
    default:
      console.log(help);
      process.exit(1)
  }
})();
