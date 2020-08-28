# rescript-vscode

The official VSCode plugin for ReScript.

## Prerequisite

`bs-platform 8.2.0` installed locally in your project.

## Installation

The plugin's on [VSCode Marketplace](https://marketplace.visualstudio.com/items?itemName=chenglou92.rescript-vscode). In VSCode, `cmd-shift-p` -> "Install Extensions", then find "rescript-vscode".

The plugin activates on `.res` and `.resi` files. If you've already got Reason-Language-Server installed, it's possible that the latter took precedence over this one. Make sure you're using this plugin ("ReScript syntax") rather than Reason-Language-Server ("BuckleScript syntax").

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

### Other Editors With Language-Server Support
This repo happens to also contain a language-server usable by other editors. If you'd like to use this language-server with e.g. Atom, for now, you have to clone the repo and run `npm run compile`. The language server will be at `server/out/server.js`. Wire that into your editor.

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

#### Change the Grammar

- Modify `grammars/rescript.tmLanguage.json`.

Currently the best way to test it is to link your extension to `~/.vscode/extensions` then reload a separate test VSCode `.res` tab after each grammar change.

For more grammar inspirations, check:
- [TypeScript's grammar](https://github.com/microsoft/TypeScript-TmLanguage/blob/a771bc4e79deeae81a01d988a273e300290d0072/TypeScript.YAML-tmLanguage)
- [Writing a TextMate Grammar: Some Lessons Learned](https://www.apeth.com/nonblog/stories/textmatebundle.html)
