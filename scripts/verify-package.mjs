#!/usr/bin/env node

/**
 * Script to verify that platform-specific native bindings and required dependencies
 * are included in the packaged .vsix file
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { execSync } from 'child_process';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const ROOT_DIR = path.join(__dirname, '..');

// Find .vsix file
const vsixFiles = fs.readdirSync(ROOT_DIR)
  .filter(f => f.endsWith('.vsix'))
  .sort((a, b) => {
    const statA = fs.statSync(path.join(ROOT_DIR, a));
    const statB = fs.statSync(path.join(ROOT_DIR, b));
    return statB.mtimeMs - statA.mtimeMs; // Most recent first
  });

if (vsixFiles.length === 0) {
  console.error('No .vsix file found. Run "npx vsce package" first.');
  process.exit(1);
}

const vsixFile = vsixFiles[0];
console.log(`Checking ${vsixFile}...\n`);

// Extract and check contents
const tempDir = path.join(ROOT_DIR, '.vsix-check');
try {
  fs.mkdirSync(tempDir, { recursive: true });
  
  // .vsix is just a zip file
  execSync(`unzip -q "${path.join(ROOT_DIR, vsixFile)}" -d "${tempDir}"`, {
    stdio: 'inherit'
  });
  
  const extensionDir = path.join(tempDir, 'extension');
  const oxcParserDir = path.join(extensionDir, 'node_modules', '@oxc-parser');
  
  // Platform-specific bindings that should be included
  const platformBindings = [
    '@oxc-parser/binding-darwin-arm64',
    '@oxc-parser/binding-darwin-x64',
    '@oxc-parser/binding-linux-x64-gnu',
    '@oxc-parser/binding-win32-x64-msvc',
  ];
  
  const checks = [
    {
      name: 'oxc-parser',
      path: path.join(extensionDir, 'node_modules', 'oxc-parser'),
      required: true
    },
    ...platformBindings.map(binding => ({
      name: binding,
      path: path.join(oxcParserDir, binding.replace('@oxc-parser/', '')),
      required: true
    }))
  ];
  
  let allGood = true;
  let foundCount = 0;
  
  for (const check of checks) {
    const exists = fs.existsSync(check.path);
    const status = exists ? '✓' : '✗';
    console.log(`${status} ${check.name}: ${exists ? 'FOUND' : 'MISSING'}`);
    
    if (exists) {
      foundCount++;
      // Check for key files in bindings
      if (check.name.startsWith('@oxc-parser/binding-')) {
        // Look for .node files (native bindings) or package.json
        const packageJson = path.join(check.path, 'package.json');
        if (fs.existsSync(packageJson)) {
          console.log(`  ✓ package.json found`);
        } else {
          console.log(`  ✗ package.json missing`);
          allGood = false;
        }
      }
    } else if (check.required) {
      allGood = false;
    }
  }
  
  console.log('');
  console.log(`Found ${foundCount}/${checks.length} required packages`);
  
  if (allGood && foundCount === checks.length) {
    console.log('✓ All required files are included in the package!');
  } else {
    console.log('✗ Some required files are missing!');
    if (foundCount < platformBindings.length) {
      console.log(`  Warning: Only ${foundCount - 1} platform bindings found, expected ${platformBindings.length}`);
      console.log('  The extension may not work on all platforms.');
    }
    process.exit(1);
  }
} finally {
  // Cleanup
  if (fs.existsSync(tempDir)) {
    fs.rmSync(tempDir, { recursive: true, force: true });
  }
}

