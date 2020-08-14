# ReScript Language Server

## Prerequisite

`bs-platform 8.2.0` installed locally in your project.

## Installation

### VSCode
Download the latest release here: https://github.com/rescript-lang/rescript-language-server/releases

Click on `rescript-language-server.vsix` to install it in VSCode.

### Other Editors With Language-Server Support
For now, you have to clone the repo and run `npm run compile`. The language server will be at `server/out/server.js`. Wire that into your editor.

## Features

- Syntax highlighting (`.res`, `.resi`).
- Formatting, with caveats:
    - Currently requires the file to be part of a ReScript project, i.e. with a `bsconfig.json`.
    - Cannot be a temporary file
- Syntax errors diagnosis (only after formatting).

### Upcoming Features
- Formatting of temporary files
- Formatting of files outside of a ReScript project root
- Type diagnosis

## Develop

### Structure

```
.
├── client // Language Client
│   ├── src
│   │   └── extension.ts // Language Client entry point
├── package.json // The extension manifest.
└── server // Language Server
    └── src
        └── server.ts // Language Server entry point
```

### Running the Project

- Run `npm install` in this folder. This installs all necessary npm modules in both the client and server folder
- Open VS Code on this folder.
- Press Ctrl+Shift+B to compile the client and server.
- Switch to the Debug viewlet (command palette -> View: Show Run and Debug).
- Select `Launch Client` from the drop down.
- Run the launch config.
- If you want to debug the server as well use the launch configuration `Attach to Server`
- In the [Extension Development Host] instance of VSCode, open a document in 'plain text' language mode.
- Try `ReScript` mode formatting with `.res` or `.resi` files.
