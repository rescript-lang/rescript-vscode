# rescript-vscode

The official VSCode plugin for ReScript.

![Screen shot](https://user-images.githubusercontent.com/1909539/101266821-790b1400-3707-11eb-8e9f-fb7e36e660e6.gif)

## Prerequisite

You **must** have `bs-platform 8.3.3` installed locally in your project, through the usual [npm installation](https://rescript-lang.org/docs/manual/latest/installation#integrate-into-existing-js-project). Older versions are not guaranteed to work.

## Recommendation

Our highlighting works well with most popular VSCode themes, such as:

- Dark+ (default dark), Light+ (default light)
- Solarized Dark, Solarized Light
- Monokai Dimmed
- Tomorrow Night Blue
- [One Dark Pro](https://marketplace.visualstudio.com/items?itemName=zhuangtongfa.Material-theme)
- [Mariana Pro](https://marketplace.visualstudio.com/items?itemName=rickynormandeau.mariana-pro)

The only 2 themes we don't (and can't) support, due to their lack of coloring, are:

- Dark (Visual Studio), Light (Visual Studio)

If your custom theme doesn't seem to highlight much (e.g. no colors for upper-case JSX tag, no distinction between module and variant), try one of the recommended themes to see if that's the problem. For more info, see [this post](https://github.com/rescript-lang/rescript-vscode/pull/8#issuecomment-764469070).

## Installation

The plugin's on [VSCode Marketplace](https://marketplace.visualstudio.com/items?itemName=chenglou92.rescript-vscode). In VSCode, `cmd-shift-p` -> "Install Extensions", then find "rescript-vscode".

The plugin activates on `.res` and `.resi` files. If you've already got Reason-Language-Server installed, it's possible that the latter took precedence over this one. Make sure you're using this plugin ("ReScript syntax") rather than Reason-Language-Server ("BuckleScript syntax").

## Features

- Supports `.res`, `.resi` and `bsconfig.json`.
- Syntax highlighting.
- Formatting, with caveats:
  - Currently requires the file to be part of a ReScript project, i.e. with a `bsconfig.json`.
  - Cannot be a temporary file.
- Syntax errors diagnosis (only after formatting).
- Built-in bsb watcher (optional, and exposed explicitly as a pop-up; no worries of dangling build).
- Type hint.
- Jump to location.
- Autocomplete.
- Snippets to ease a few syntaxes:
  - `external` features such as `@bs.module` and `@bs.val`
  - `try`, `for`, etc.
- Folding, and [custom folding](https://code.visualstudio.com/docs/editor/codebasics#_folding) through `//#region` and `//#endregion`.

### Upcoming Features

- Formatting of temporary files
- Formatting of files outside of a ReScript project root
