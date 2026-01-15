import * as fs from "fs";
import * as path from "path";

export const normalizePath = (filePath: string | null): string | null => {
  return filePath != null ? path.normalize(filePath) : null;
};

export const findProjectRootOfFileInDir = (source: string): string | null => {
  const normalizedSource = normalizePath(source);
  if (normalizedSource == null) {
    return null;
  }

  const dir = normalizePath(path.dirname(normalizedSource));
  if (dir == null) {
    return null;
  }

  if (
    fs.existsSync(path.join(dir, "rescript.json")) ||
    fs.existsSync(path.join(dir, "bsconfig.json"))
  ) {
    return dir;
  }

  if (dir === normalizedSource) {
    return null;
  }

  return findProjectRootOfFileInDir(dir);
};

export const findProjectRootOfFile = (source: string): string | null => {
  const normalizedSource = normalizePath(source);
  if (normalizedSource == null) {
    return null;
  }

  return findProjectRootOfFileInDir(normalizedSource);
};
