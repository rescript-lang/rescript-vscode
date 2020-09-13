import * as path from 'path';

// See https://microsoft.github.io/language-server-protocol/specification Abstract Message
// version is fixed to 2.0
export let jsonrpcVersion = '2.0';
export let bscPartialPath = path.join('node_modules', 'bs-platform', process.platform, 'bsc.exe');
export let compilerLogPartialPath = path.join('lib', 'bs', '.compiler.log');
export let resExt = '.res';
export let resiExt = '.resi';
