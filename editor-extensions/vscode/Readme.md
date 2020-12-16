# vscode-reason-language-server

Based on `vscode-reasonml`, but with a language server backend that's written entirely in reason & compiled natively.

![screenshot](https://github.com/jaredly/reason-language-server/raw/master/editor-extensions/vscode/screenshot.png)

## Features

- hover goodness
- autocomplete
- function signature help
- jump-to-definition & preview-definition
- "Find all references"
- document outline / symbol search
- code lens

  - Show a file's dependencies at the top
  - Show what values are used from an `open`
  - Per-value type codelens (off by default)

- highlight all usages of a variable
- rename a variable
- format selection
- format document
- auto-run bsb / dune on file change

## Configuration
all configuration is prefixed with `reason_language_server.`

## Debugging configuration
most useful if your developing the language server

- `.location` - provide a custom binary location for the langauge server
- `.reloadOnChange` - reload the server when the binary is updated
- `.show_debug_errors` - pipe the server's stderr into vscode's output pane

## Copyright & License

Copyright © 2018 Jared Forsyth and contributors.

Distributed under the MIT License (see [LICENSE](./LICENSE)).
