#!/usr/bin/env node
import fs from "fs";
import server from "./server";

const args = process.argv.slice(2)

const help = `ReScript Language Server

Usage: rescript-language-server [options]

Options:

--stdio               Use stdio
--node-ipc            Use node-ipc
--dont-ask            Don't ask to start build (use it as second parameter)
-v, --version         Print version
-h, --help            Print help`;

(() => {
  switch (args[0]) {
    case '--stdio':
      return server(true, args[1] !== "--dont-ask");
    case '--node-ipc':
      return server(false, args[1] !== "--dont-ask");
    case '--version':
    case '-v':
      console.log(JSON.parse(fs.readFileSync('./package.json', { encoding: 'utf8' })).version);
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
