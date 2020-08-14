# ReScript Language Server

## Installation

### VSCode
Download the latest release here: https://github.com/rescript-lang/rescript-language-server/releases

Click on `rescript-language-server.vsix` to install it in VSCode.

### Other Editors With Language-Server Support
For now, you have to clone the repo and run `npm run compile`. The language server will be at `server/out/server.js`. Wire that into your editor.

## Features

- Syntax highlighting (`.res`, `.resi`).
- Formatting (currently requires the file to be part of a ReScript project, i.e. with a `bsconfig.json`).

### Upcoming Features
- Formatting of temporary files
- Type diagnosis
