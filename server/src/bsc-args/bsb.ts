import * as path from "path";
import fs from "fs";
import { IncrementallyCompiledFileInfo } from "../incrementalCompilation";
import { buildNinjaPartialPath } from "../constants";

export type BsbCompilerArgs = string[];

// TODO: I guess somewhere in here, when the version is v12 beta 10 or later,
// We need to pass -rescript-runtime as argument as well.

export async function getBsbBscArgs(
  entry: IncrementallyCompiledFileInfo,
): Promise<BsbCompilerArgs | null> {
  const buildNinjaPath = path.resolve(
    entry.project.rootPath,
    buildNinjaPartialPath,
  );

  let stat: fs.Stats;
  try {
    stat = await fs.promises.stat(buildNinjaPath);
  } catch {
    return null;
  }

  const cache = entry.buildNinja;
  if (cache && cache.fileMtime >= stat.mtimeMs) {
    return cache.rawExtracted;
  }

  const fh = await fs.promises.open(buildNinjaPath, "r");
  try {
    let captureNext = false;
    let haveAst = false;
    const captured: string[] = [];

    for await (const rawLine of fh.readLines()) {
      const line = String(rawLine).trim();
      if (captureNext) {
        captured.push(line);
        captureNext = false;
        if (haveAst && captured.length === 2) break; // got ast + mij
      }
      if (line.startsWith("rule astj")) {
        captureNext = true;
        haveAst = true;
      } else if (line.startsWith("rule mij")) {
        captureNext = true;
      }
    }

    if (captured.length !== 2) return null;

    entry.buildNinja = {
      fileMtime: stat.mtimeMs,
      rawExtracted: captured,
    };
    return captured;
  } finally {
    await fh.close();
  }
}
