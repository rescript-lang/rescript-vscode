#!/usr/bin/env node

/**
 * Script to verify that the WASM binding and required dependencies
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
  
  // Check for WASM binding
  const wasmBindingPath = path.join(extensionDir, 'node_modules', '@oxc-parser', 'binding-wasm32-wasi');
  const wasmRuntimePath = path.join(extensionDir, 'node_modules', '@napi-rs', 'wasm-runtime');
  const oxcParserPath = path.join(extensionDir, 'node_modules', 'oxc-parser');
  
  const checks = [
    {
      name: 'WASM binding',
      path: wasmBindingPath,
      required: true
    },
    {
      name: 'WASM runtime',
      path: wasmRuntimePath,
      required: true
    },
    {
      name: 'oxc-parser',
      path: oxcParserPath,
      required: true
    }
  ];
  
  let allGood = true;
  for (const check of checks) {
    const exists = fs.existsSync(check.path);
    const status = exists ? '✓' : '✗';
    console.log(`${status} ${check.name}: ${exists ? 'FOUND' : 'MISSING'}`);
    
    if (exists) {
      // Check for key files
      if (check.name === 'WASM binding') {
        const parserFile = path.join(check.path, 'parser.wasi.cjs');
        if (fs.existsSync(parserFile)) {
          console.log(`  ✓ parser.wasi.cjs found`);
        } else {
          console.log(`  ✗ parser.wasi.cjs missing`);
          allGood = false;
        }
      }
    } else if (check.required) {
      allGood = false;
    }
  }
  
  console.log('');
  if (allGood) {
    console.log('✓ All required files are included in the package!');
  } else {
    console.log('✗ Some required files are missing!');
    process.exit(1);
  }
} finally {
  // Cleanup
  if (fs.existsSync(tempDir)) {
    fs.rmSync(tempDir, { recursive: true, force: true });
  }
}

