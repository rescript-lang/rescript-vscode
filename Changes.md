## master
- Add support for autocomplete for `foo->`: the type of `foo` is used to determine the module to take completions from.
- Add support for autocomplete for decorators such as `@module` and `@val`.
- Fix issue for uncurried functions where the internal definition of `Js.Fn.arity` is shown on hover. (See https://github.com/rescript-lang/rescript-editor-support/issues/62).
- Fix type hint when hovering over labeled arguments of components (See https://github.com/rescript-lang/rescript-editor-support/issues/63).
- Fix issue where values declared with type annotation would not show up in autocomplete, and would show no doc comment on hover. (See https://github.com/rescript-lang/rescript-vscode/issues/72).


## Release 1.0.5 of rescript-vscode
This [commit](https://github.com/rescript-lang/rescript-editor-support/commit/6bdd10f6af259edc5f9cbe5b9df06836de3ab865) is vendored in [rescript-vscode 1.0.5](https://github.com/rescript-lang/rescript-vscode/releases/tag/1.0.5).

- Add support for doc strings when hovering on modules.
- Add support for printing uncurried function types in hover.
- Fix autocomplete issue where `open Foo` would be picked up inside line comments (see https://github.com/rescript-lang/rescript-editor-support/issues/15).
- Don't print parens as in `A()` for 0-ary variants.
- Fix infinite loop in autocomplete that can cause `rescript-editor-support.exe` processes to use up 100% cpu.
- Fix jump to type definition for types defined in an inner module.

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
