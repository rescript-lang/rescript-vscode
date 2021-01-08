## master
- Add support for doc strings when hovering on modules.

## Release 1.0.3 of rescript-vscode 
This [commit](https://github.com/rescript-lang/rescript-editor-support/commit/214d220d8573f9f0c8d54e623c163e01617bf124) is vendored in [rescript-vscode 1.0.3](https://github.com/rescript-lang/rescript-vscode/releases/tag/1.0.3).

- Fix type shown when hovering on record fields (see https://github.com/rescript-lang/rescript-vscode/issues/52), and doc comments for records.
- Fix issue where type variables are printed with global renaming when hovering or autocompleting a module (see https://github.com/rescript-lang/rescript-editor-support/issues/38).
- Fix issue where a log file was always created (see https://github.com/rescript-lang/rescript-vscode/issues/47).
- Add support for hover on the id of toplevel module definitions (```module Id = ...```).

## Release 1.0.1 of rescript-vscode 
This [commit](https://github.com/rescript-lang/rescript-editor-support/commit/232ad609766c415048750c5cc828973a9995f382) is vendored in [rescript-vscode 1.0.1](https://github.com/rescript-lang/rescript-vscode/releases/tag/1.0.1).

- Support printing inline records.
- Add typedef hover support.
- Always output valid json, even in case of internal error.
- Remove semicolon in module top level preview.
- Support syntax highlight in hover fenced blocks.
- Fix printing of variant arguments.
- Use outcome printer from the syntax to print type declarations.
- Fix issue in command-line parsing on Windows with paths of the form `c:/...:line:column`.

## Release 1.0.0 of rescript-vscode 
This [commit](https://github.com/rescript-lang/rescript-editor-support/commit/d45f45793a307a3bb87dcac0542fd412669f1b6e) is vendored in [rescript-vscode 1.0.0](https://github.com/rescript-lang/rescript-vscode/releases/tag/1.0.0).
