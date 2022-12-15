#!/usr/bin/env node
import fs from "fs";
import server from "./server";

const args = process.argv.slice(2)

const help = `ReScript Language Server

Options:

--version   Print version
--help      Print help`;

(() => {
  if (args.length === 0) {
    return server();
  }

  switch (args[0]) {
    case '--version':
      console.log(JSON.parse(fs.readFileSync('./package.json', { encoding: 'utf8' })).version);
      process.exit(0);
    case '--help':
      console.log(help);
      process.exit(0);
    default:
      console.log(help);
      process.exit(1)
  }
})();