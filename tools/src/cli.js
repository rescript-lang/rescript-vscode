#!/usr/bin/env node
//@ts-check
"use strict";

const child_process = require("child_process");
const path = require("path");

const platformArch =
  process.arch === "x64" ? process.platform : process.platform + process.arch;

const binPath = path.join(__dirname, "..", "binaries", platformArch, "rescript-tools.exe");

const args = process.argv.slice(2);

const spawn = child_process.spawnSync(binPath, args);

if (spawn.status !== 0) {
  console.log(spawn.stderr.toString().trim());
  process.exit(spawn.status || 1);
} else {
  console.log(spawn.stdout.toString().trim());
  process.exit(0)
}
