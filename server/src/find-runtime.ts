import { readdir, stat as statAsync } from "fs/promises";
import { join, resolve } from "path";

// Efficient parallel folder traversal to find node_modules directories
async function findNodeModulesDirs(
  rootPath: string,
  maxDepth = 12,
): Promise<string[]> {
  const nodeModulesDirs: string[] = [];
  const stack: Array<{ dir: string; depth: number }> = [
    { dir: rootPath, depth: 0 },
  ];
  const visited = new Set<string>();

  while (stack.length) {
    const { dir, depth } = stack.pop()!;
    if (depth > maxDepth || visited.has(dir)) continue;
    visited.add(dir);

    let entries: string[];
    try {
      entries = await readdir(dir);
    } catch {
      continue;
    }

    if (entries.includes("node_modules")) {
      const nm = join(dir, "node_modules");
      try {
        const st = await statAsync(nm);
        if (st.isDirectory()) {
          nodeModulesDirs.push(nm);
          // Do NOT push deeper here to keep same behavior (stop at first node_modules in this branch)
          continue;
        }
      } catch {}
    }

    for (const entry of entries) {
      if (entry === "node_modules" || entry.startsWith(".")) continue;
      const full = join(dir, entry);
      try {
        const st = await statAsync(full);
        if (st.isDirectory()) {
          stack.push({ dir: full, depth: depth + 1 });
        }
      } catch {}
    }
  }

  return nodeModulesDirs;
}

// Custom function to find Deno vendorized @rescript/runtime directories
async function findDenoRescriptRuntime(nodeModulesPath: string) {
  // We only care about the Deno vendorized layout:
  // <nodeModulesPath>/.deno/@rescript+runtime@<version>/node_modules/@rescript/runtime
  const denoRoot = join(nodeModulesPath, ".deno");
  let entries: string[];
  try {
    entries = await readdir(denoRoot);
  } catch {
    return [];
  }

  // Collect all @rescript+runtime@<version> vendor dirs
  const vendorDirs = entries.filter((e) => e.startsWith("@rescript+runtime@"));
  if (vendorDirs.length === 0) return [];

  // Optionally pick “latest” by version; for now we return all valid matches.
  const results: string[] = [];
  for (const dir of vendorDirs) {
    const runtimePath = join(
      denoRoot,
      dir,
      "node_modules",
      "@rescript",
      "runtime",
    );
    try {
      const st = await statAsync(runtimePath);
      if (st.isDirectory()) results.push(runtimePath);
    } catch {
      // Ignore inaccessible / missing path
    }
  }

  return results;
}

async function findRuntimePath(project: string) {
  // Find all node_modules directories using efficient traversal
  const node_modules = await findNodeModulesDirs(project);

  const rescriptRuntimeDirs = await Promise.all(
    node_modules.map(async (nm) => {
      const results = [];

      // Check for standard layout: @rescript/runtime
      const standardPath = join(nm, "@rescript", "runtime");
      try {
        const stat = await statAsync(standardPath);
        if (stat.isDirectory()) {
          results.push(standardPath);
          // If we found standard layout, no need to search for Deno layouts
          return results;
        }
      } catch (e) {
        // Directory doesn't exist, continue
      }

      // Only check for Deno vendorized layouts if standard layout wasn't found
      const denoResults = await findDenoRescriptRuntime(nm);
      results.push(...denoResults);

      return results;
    }),
  ).then((results) => results.flatMap((x) => x));

  return rescriptRuntimeDirs.map((runtime) => resolve(runtime));
}

function findRuntimeCached() {
  const cache = new Map<string, string[]>();
  return async (project: string) => {
    if (cache.has(project)) {
      return cache.get(project)!;
    }
    const runtimes = await findRuntimePath(project);
    cache.set(project, runtimes);
    return runtimes;
  };
}

/**
 * Find all installed @rescript/runtime directories in the given project path.
 * In a perfect world, there should be exactly one.
 * This function is cached per project path.
 */
export const findRescriptRuntimesInProject = findRuntimeCached();
