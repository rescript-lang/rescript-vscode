<h1 align="center">
  <a href="https://marketplace.visualstudio.com/items?itemName=chenglou92.rescript-vscode">ReScript VSCode</a>
</h1>

<p align="center">The Official VSCode plugin for ReScript</p>

<p align="center">
  <img src="https://user-images.githubusercontent.com/1909539/101266821-790b1400-3707-11eb-8e9f-fb7e36e660e6.gif"/>
</p>

## Contents

- [Contents](#contents)
- [📝 Prerequisite](#-prerequisite)
- [🌈 Supported Themes](#-supported-themes)
- [💡 Features](#-features)
- [📥 Installation](#-installation)
  - [Pre-release channel](#pre-release-channel)
- [📦 Commands](#-commands)
- [🔨 Settings](#-settings)
- [🚀 Code Analyzer](#-code-analyzer)
  - [Configuring the Code Analyzer](#configuring-the-code-analyzer)
  - [Usage](#usage)
  - [Caveats](#caveats)
- [🪄 Tips \& Tricks](#-tips--tricks)
  - [Hide generated files](#hide-generated-files)
- [📰 Changelog](#-changelog)
- [👏 How to Contribute](#-how-to-contribute)
- [📄 License](#-license)

## 📝 Prerequisite

You **must** have [ReScript](https://www.npmjs.com/package/rescript) >= 9.1 installed locally in your project, through the usual [npm or yarn installation](https://rescript-lang.org/docs/manual/latest/installation#integrate-into-existing-js-project). Older versions are not guaranteed to work.

## 🌈 Supported Themes

Our highlighting works well with most popular VSCode themes, such as:

- Dark+ (default dark), Light+ (default light)
- Solarized Dark, Solarized Light
- Monokai Dimmed
- Tomorrow Night Blue
- [One Dark Pro](https://marketplace.visualstudio.com/items?itemName=zhuangtongfa.Material-theme)

The only 2 themes we don't (and can't) support, due to their lack of coloring, are:

- Dark (Visual Studio), Light (Visual Studio)

> **Note**
> If your custom theme doesn't seem to highlight much (e.g. no colors for upper-case JSX tag, no distinction between module and variant), try one of the recommended themes to see if that's the problem. For more info, see [this post](https://github.com/rescript-lang/rescript-vscode/pull/8#issuecomment-764469070).

## 💡 Features

- Supports `.res`, `.resi`, `rescript.json` and the legacy config file `bsconfig.json`.
- Syntax highlighting.
- Formatting.
- Build diagnostics.
- Built-in bsb watcher (optional, and exposed explicitly as a pop-up; no worries of dangling build).
- Type hint hover.
- Jump to definition.
- Autocomplete.
- Find references.
- Rename.
- Inlay Hints.
- Signature help.
- Code lenses.
- Snippets to ease a few syntaxes:
  - `external` features such as `@bs.module` and `@bs.val`
  - `try`, `for`, etc.
- Folding, and [custom folding](https://code.visualstudio.com/docs/editor/codebasics#_folding) through `//#region` and `//#endregion`.

## 📥 Installation

Launch VS Code Quick Open (`Ctrl+P`), paste the following command, and press enter.

```
ext install chenglou92.rescript-vscode
```

The plugin activates on `.res` and `.resi` files. If you've already got Reason-Language-Server installed, it's possible that the latter took precedence over this one. Make sure you're using this plugin ("ReScript syntax") rather than Reason-Language-Server ("BuckleScript syntax").

### Pre-release channel

There is a pre-release channel available. It is intended for testing new and therefore possibly unstable features. You can activate it by clicking on the "Switch to Pre-Release Version" button on the `rescript-vscode` extension page in VSCode. From this point on, pre-release versions will always have an odd version minor (1.5.x, 1.7.x, 2.1.x, etc.) while stable releases have even version minor numbers (1.4.x, 1.6.x, 2.0.0, etc.).

Even if the pre-release channel seems too experimental to you, we still suggest you to give it a try and submit any issues that you run into. In the long run it will give us a better editor experience overall.

## 📦 Commands

| Command                                                          | Description                                                                                                                                                                                                                                                                                           |
| ---------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| ReScript: Create an interface file for this implementation file  | Creates an interface file (`.resi`) for the current `.res` file, automatically filling in all types and values in the current file.                                                                                                                                                                   |
| ReScript: Open the compiled JS file for this implementation file | Opens the compiled JS file for the current ReScript file.                                                                                                                                                                                                                                             |
| ReScript: Switch implementation/interface                        | Switches between the implementation and interface file. If you're in a `.res` file, the command will open the corresponding `.resi` file (if it exists), and if you're in a `.resi` file the command will open the corresponding `.res` file. This can also be triggered with the keybinding `Alt+O`. |
| ReScript: Start Code Analyzer                                    | This will start code analysis in the ReScript project of the file you run the command from.                                                                                                                                                                                                           |

## 🔨 Settings

You'll find all ReScript specific settings under the scope `rescript.settings`.

| Setting                    | Description                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| -------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Prompt to Start Build      | If there's no ReScript build running already in the opened project, the extension will prompt you and ask if you want to start a build automatically. You can turn off this automatic prompt via the setting `rescript.settings.askToStartBuild`.                                                                                                                                                                                                                                                                                |
| ReScript Binary Path       | The extension will look for the existence of a `node_modules/.bin/rescript` file and use its directory as the `binaryPath`. If it does not find it at the project root (which is where the nearest `rescript.json` resides), it goes up folders in the filesystem recursively until it either finds it (often the case in monorepos) or hits the top level. To override this lookup process, the path can be configured explicitly using the setting `rescript.settings.binaryPath`                                              |
| ReScript Platform Path     | The extension will look for the existence of a `node_modules/rescript` directory and use the subdirectory corresponding to the current platform as the `platformPath`. If it does not find it at the project root (which is where the nearest `rescript.json` resides), it goes up folders in the filesystem recursively until it either finds it (often the case in monorepos) or hits the top level. To override this lookup process, the path can be configured explicitly using the setting `rescript.settings.platformPath` |
| ReScript Runtime Path      | The extension will look for the existence of a `node_modules/@rescript/runtime` directory (ReScript v12 beta 11+). To override this lookup process, the path can be configured explicitly using the setting `rescript.settings.runtimePath`.                                                                                                                                                                                                                                                                                     |
| Inlay Hints (experimental) | This allows an editor to place annotations inline with text to display type hints. Enable using `rescript.settings.inlayHints.enable: true`                                                                                                                                                                                                                                                                                                                                                                                      |
| Code Lens (experimental)   | This tells the editor to add code lenses to function definitions, showing its full type above the definition. Enable using `rescript.settings.codeLens: true`                                                                                                                                                                                                                                                                                                                                                                    |
| Signature Help             | This tells the editor to show signature help when you're writing function calls. Enable using `rescript.settings.signatureHelp.enabled: true`                                                                                                                                                                                                                                                                                                                                                                                    |
| Compile Status Indicator   | Shows compile status in the status bar (Compiling, Errors, Warnings, Success). Toggle via `rescript.settings.compileStatus.enable`. Clicking in Error/Warning modes focuses the Problems view.                                                                                                                                                                                                                                                                                                                                   |

**Default settings:**

```jsonc
// Whether you want the extension to prompt for autostarting a ReScript build if a project is opened with no build running
"rescript.settings.askToStartBuild": true,

// Path to the directory where cross-platform ReScript binaries are. You can use it if you haven't or don't want to use the installed ReScript from node_modules in your project.
"rescript.settings.binaryPath": null

// Path to the directory where platform-specific ReScript binaries are. You can use it if you haven't or don't want to use the installed ReScript from node_modules in your project.
"rescript.settings.platformPath": null

// Enable (experimental) inlay hints.
"rescript.settings.inlayHints.enable": true

// Maximum length of character for inlay hints. Set to null to have an unlimited length. Inlay hints that exceed the maximum length will not be shown
"rescript.settings.inlayHints.maxLength": 25

// Enable (experimental) code lens for function definitions.
"rescript.settings.codeLens": true

// Show compile status in the status bar (compiling/errors/warnings/success)
"rescript.settings.compileStatus.enable": true
```

## 🚀 Code Analyzer

The Code Analyzer is a mode in the extension that runs additional code analysis in your project. The analysis helps you find dead code at a granular level, find unhandled exceptions, and more.

> The Code Analyzer uses [reanalyze](https://github.com/rescript-association/reanalyze), which is embedded in the extension, so you don't need to install anything extra to run it.

### Configuring the Code Analyzer

You'll need to configure what code analysis you want to run, and what (if any) directories you want to ignore. Configuration is done via adding `reanalyze` in `rescript.json`. You'll get autocomplete for what configuration options are valid. You can also read [all about configuring `reanalyze` here](https://github.com/rescript-association/reanalyze#configuration-via-bsconfigjson).

### Usage

Open the command palette and run `ReScript: Start Code Analyzer`. This will start code analysis in the ReScript project of the file you run the command from.

Dead code is highlighted in the editor, and code actions for suppressing dead code warnings are available in most cases.

When done, stop the code analysis mode by clicking the `Stop Code Analyzer` button in the editor status bar. This will clear all reported analysis warnings.

### Caveats

Currently does not work for full monorepo dead code analysis (although it should work for each monorepo package individually).

## 🪄 Tips & Tricks

### Hide generated files

You can configure VSCode to collapse the JavaScript files ReScript generates under its source ReScript file. This will "hide" the generated files in the VSCode file explorer, but still leaving them accessible by expanding the source ReScript file they belong to.

Open your VSCode settings and type:

```jsonc
"explorer.fileNesting.enabled": true,
"explorer.fileNesting.patterns": {
  "*.res": "${capture}.mjs, ${capture}.js, ${capture}.cmi, ${capture}.cmt, ${capture}.cmj",
  "*.resi": "${capture}.res"
},
```

This nests implementations under interfaces if they're present and nests all generated files under the main ReScript file. Adapt and tweak to your liking.

A screenshot of the result:

![Shows the end result in VSCode, with ReScript related files nested under eachother appropriately.](https://user-images.githubusercontent.com/1457626/168123647-400e2f09-31e3-45a2-b74b-190c7c207446.png)

## 📰 Changelog

See [CHANGELOG](CHANGELOG.md)

## 👏 How to Contribute

Read our [Contributing Guide](CONTRIBUTING.md)

## 📄 License

See the [LICENSE](./LICENSE.txt) file for details.
