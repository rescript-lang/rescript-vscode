#!/usr/bin/env node

/**
 * Script to download and install oxc-parser WASM binding
 * without platform checks by downloading tarball directly from npm registry
 */

import https from 'https';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { execSync } from 'child_process';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const ROOT_DIR = path.join(__dirname, '..');
const NODE_MODULES_DIR = path.join(ROOT_DIR, 'node_modules');
const OXCPARSER_DIR = path.join(NODE_MODULES_DIR, '@oxc-parser');
const PACKAGE_JSON = path.join(ROOT_DIR, 'package.json');

// Read oxc-parser version from package.json
const packageJson = JSON.parse(fs.readFileSync(PACKAGE_JSON, 'utf-8'));
const oxcParserVersion = packageJson.dependencies['oxc-parser']?.replace('^', '') || 
                         packageJson.dependencies['oxc-parser'] || 
                         '0.97.0';
const WASM_PACKAGE = `@oxc-parser/binding-wasm32-wasi@${oxcParserVersion}`;

async function fetchPackageInfo(packageName) {
  // npm registry expects scoped packages as @scope%2Fname
  const packagePath = packageName.replace('/', '%2F');
  const url = `https://registry.npmjs.org/${packagePath}`;
  
  return new Promise((resolve, reject) => {
    https.get(url, (res) => {
      if (res.statusCode !== 200) {
        reject(new Error(`HTTP ${res.statusCode} when fetching ${url}`));
        return;
      }
      
      let data = '';
      res.on('data', (chunk) => { data += chunk; });
      res.on('end', () => {
        try {
          const json = JSON.parse(data);
          if (json.error) {
            reject(new Error(`npm registry error: ${json.error}`));
            return;
          }
          resolve(json);
        } catch (e) {
          reject(new Error(`Failed to parse JSON: ${e.message}. Response: ${data.substring(0, 200)}`));
        }
      });
    }).on('error', (err) => {
      reject(new Error(`Network error fetching ${url}: ${err.message}`));
    });
  });
}

async function downloadAndExtract(packageSpec) {
  // Parse package spec: @scope/name@version
  const lastAtIndex = packageSpec.lastIndexOf('@');
  const packageName = packageSpec.substring(0, lastAtIndex);
  const version = packageSpec.substring(lastAtIndex + 1);
  
  console.log(`Installing ${packageName}@${version}...`);
  
  try {
    const packageInfo = await fetchPackageInfo(packageName);
    
    if (!packageInfo || !packageInfo.versions) {
      throw new Error(`Failed to fetch package info for ${packageName}. Response: ${JSON.stringify(packageInfo).substring(0, 200)}`);
    }
    
    // Handle version ranges - get the latest matching version
    let versionToUse = version;
    if (version.startsWith('^') || version.startsWith('~')) {
      // For ranges, use the latest version from the registry
      const versions = Object.keys(packageInfo.versions).sort((a, b) => {
        return b.localeCompare(a, undefined, { numeric: true });
      });
      versionToUse = versions[0];
      console.log(`  Resolved version range ${version} to ${versionToUse}`);
    }
    
    const versionData = packageInfo.versions[versionToUse];
    
    if (!versionData) {
      const availableVersions = Object.keys(packageInfo.versions).slice(0, 5).join(', ');
      throw new Error(`Version ${versionToUse} not found for ${packageName}. Available versions: ${availableVersions}...`);
    }
    
    const tarballUrl = versionData.dist.tarball;
    const tarballPath = path.join(ROOT_DIR, 'binding-wasm32-wasi.tgz');
    const packageDirName = packageName.replace('@oxc-parser/', '');
    const extractDir = path.join(OXCPARSER_DIR, packageDirName);
    
    // Create directory structure
    fs.mkdirSync(OXCPARSER_DIR, { recursive: true });
    
    // Download tarball
    const file = fs.createWriteStream(tarballPath);
    await new Promise((resolve, reject) => {
      https.get(tarballUrl, (response) => {
        response.pipe(file);
        file.on('finish', () => {
          file.close();
          resolve();
        });
      }).on('error', reject);
    });
    
    // Extract using tar (assuming tar is available)
    fs.mkdirSync(extractDir, { recursive: true });
    execSync(`tar -xzf "${tarballPath}" -C "${extractDir}" --strip-components=1`, {
      stdio: 'inherit'
    });
    
    // Clean up tarball
    fs.unlinkSync(tarballPath);
    
    console.log(`✓ Installed ${packageName}@${version}`);
  } catch (error) {
    console.error(`✗ Failed to install ${packageSpec}:`, error.message);
    throw error;
  }
}

async function main() {
  console.log('Installing WASM binding...');
  
  // Ensure node_modules/@oxc-parser exists
  fs.mkdirSync(OXCPARSER_DIR, { recursive: true });
  
  await downloadAndExtract(WASM_PACKAGE);
  
  console.log('✓ WASM binding installed to node_modules/@oxc-parser/binding-wasm32-wasi/');
}

main().catch((error) => {
  console.error('Error:', error);
  process.exit(1);
});

