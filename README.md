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
- Built-in bsb watcher (optional, and exposed explicitly as a pop-up; no worries of dangling build)
- Type diagnosis
- Jump to location
- Formatting of temporary files
- Formatting of files outside of a ReScript project root

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

- Run `npm install` at the root. This will also install the necessary npm modules in both the `client` and `server` folders.
- Open VS Code to this folder.
- Switch to the Debug viewlet (command palette -> View: Show Run and Debug).
- Select `Client + Server` from the drop down, launch it (green arrow):

  <img width="235" alt="image" src="https://user-images.githubusercontent.com/1909539/97448097-7d186a80-18ed-11eb-82d6-d55b70f54811.png">

  (If you're getting some Promise-related error alert, file an issue here so that we can find a workaround; this seems to be a VSCode bug).
- In the [Extension Development Host] instance of VSCode that just opened, open a `.res` file.
- Try various features.
- When you make a change, Go to the same Debug viewlet's Call Stack panel and restart the client and the server:

  <img width="359" alt="image" src="https://user-images.githubusercontent.com/1909539/97448639-19db0800-18ee-11eb-875a-d17cd1b141d1.png">

#### Change the Grammar

- Modify `grammars/rescript.tmLanguage.json`.

For more grammar inspirations, check:
- [TypeScript's grammar](https://github.com/microsoft/TypeScript-TmLanguage/blob/a771bc4e79deeae81a01d988a273e300290d0072/TypeScript.YAML-tmLanguage)
- [Writing a TextMate Grammar: Some Lessons Learned](https://www.apeth.com/nonblog/stories/textmatebundle.html)
