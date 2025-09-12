// benchmark
const start = process.hrtime.bigint();

// start code
const args = process.argv.slice(2);

if (args.length === 0) {
  console.log(`
Usage: node find-runtime.mjs <project-folder>
Find @rescript/runtime directories in a project's node_modules.
Arguments:
  project-folder    Path to the project directory to search
Examples:
  node find-runtime.mjs /path/to/project
  node find-runtime.mjs .
`);
  process.exit(1);
}

const project = args[args.length - 1];

import { findRescriptRuntimesInProject } from "../server/src/find-runtime.ts";

const runtimes = await findRescriptRuntimesInProject(project);

console.log("Found @rescript/runtime directories:", runtimes);

// end code
const end = process.hrtime.bigint();
const durationMs = Number(end - start) / 1e6; // convert ns → ms

console.log(`Script took ${durationMs.toFixed(3)}ms`);
