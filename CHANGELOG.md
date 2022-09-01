# Changelog

> **Tags:**
>
> - :boom: [Breaking Change]
> - :eyeglasses: [Spec Compliance]
> - :rocket: [New Feature]
> - :bug: [Bug Fix]
> - :memo: [Documentation]
> - :house: [Internal]
> - :nail_care: [Polish]

## master

#### :rocket: New Feature

- Inlay Hints (experimetal). `rescript.settings.inlayHints.enable: true`. Turned off by default. https://github.com/rescript-lang/rescript-vscode/pull/453
- Code Lenses for functions (experimetal). `rescript.settings.codeLens: true`. Turned off by default. https://github.com/rescript-lang/rescript-vscode/pull/513
- Markdown code blocks tagged as `rescript` now get basic syntax highlighting. https://github.com/rescript-lang/rescript-vscode/pull/97
- Hover support for doc comments on v10 compiler `/** this is a doc comment */`

#### :bug: Bug Fix

- Fix issue where autocomplete would not perform type instantiation https://github.com/rescript-lang/rescript-vscode/pull/561
- Fix issue where hovering over a field in record construction would show the type without instantiating its type arguments https://github.com/rescript-lang/rescript-vscode/pull/560
- Fix Incorrect semantic highlighting of `external` declarations https://github.com/rescript-lang/rescript-vscode/pull/517
- Fix issue where doc comment with nested comments inside is not shown properly on hover https://github.com/rescript-lang/rescript-vscode/pull/526
- Fix server crashes when open file is removed from disk with inlayHints enabled https://github.com/rescript-lang/rescript-vscode/issues/538
- Fix inlay hint for destructured record/array https://github.com/rescript-lang/rescript-vscode/issues/536

## v1.4.2

#### :bug: Bug Fix

- Fix finding the ReScript bin for even more kinds of setups.
- Document the process of finding the ReScript bin in README.

## v1.4.1

#### :bug: Bug Fix

- Fix formatting not preferring the locally installed formatter (if available), which made formatting potentially different between formatting via `rescript format` and the extension.
- Fix finding the ReScript bin in monorepos

## v1.4.0

#### :rocket: New Feature

- Add command: ReScript: Restart Language Server
- Extend integration with reanalyze for code analysis. Support both `dce` and `exception` analysis which can be configured to e.g. both run by adding `{"reanalyze": {"analysis": ["dce", "exception"]} }` in `bsconfig.json`.
- Add configuration option for suppressing the "Do you want to start a build?" prompt.
- Add configuration option for autostarting the Code Analyzer.
- Report syntax errors as you type.

#### :bug: Bug Fix

- Fix issue with autocompletion for constructors in switch statements.
- Fix issue with autocompletion inside template expressions.
- Fix handling of local opens.
- Fix extension crash when renaming a file.
- Fix issue where the server would crash on genType's errors.
- Fix issue where the server would crash if the project contains an OCaml file with a syntax error.
- Fix issue where `@inline` was not suported by the command to generate an interface file.

#### :nail_care: Polish

- Add hover information with links to documentation for decorators.
- Sync with latest parser/printer.

#### :house: Internal

- Support paths to rescript executables in arm64 architectures.

#### :boom: Breaking Change

- Drop support for `bs-patform`. Only `rescript` supported.

## 1.3.0

- Fix issue where using paths of the form `./something` would show multiple copies of the same file in vscode.
- When hovering on a field access, show the instantiated type of the field.
- Support autocomplete for objects from another module `M.x[...`.
- Fix command for creating interface files when the project uses namespaces.
- Added command `ReScript: Open the compiled JS file for this implementation file.`.
- Use semantic syntax highlighting (https://github.com/rescript-lang/rescript-vscode/pull/367).
- Report "Fatal error" when it happens in the compiler log (e.g. a make function with type annotation) and don't crash the extension.
- Fix issue in functions the form "~foo as name" where the location would only cover "ame".
- Extend the command to create an interface file, to support components and ReScript decorators used in bindings.
- Enable formatting files without needing the file to be in an actual ReScript project.
- New feature: Show Outline which was previously disabled.
- Add command to quickly switch between implementation and interface file.
- Support hover on JSX props and labelled arguments.

## 1.2.1

- Fix issue with highlighting of interpolation strings (those with backticks) introduced in release 1.2.0.
- Fix crash when the project contains OCaml files that have warnings.
- Fix crash on hover when a dependency contains a type with functor application. This is not expressible in ReScript syntax, but can appear in a dependent OCaml package and be pulled in for processing by the extension.
- Remove obsolete `@bs` snippets

## 1.2.0

Features:

- Add autocompletion for object access of the form `foo["x"]` and `foo["x"]["y"]["z"]`.
- Support autocomplete of records for variables defined in other files.
- Improve autocomplete when several values have the same name, with a heuristic to approximate the correct scope.
- Add a "Dead Code Analysis" mode that will highlight globally dead values, redundant optional arguments, dead modules, dead types (records and variants) ([#334](https://github.com/rescript-lang/rescript-vscode/pull/334))

Fixes:

- Fix issue in JSX autocomplete when the component is declared external.
- Fix jump-to-definition for uncurried calls.
- Fix issue where values for autocomplete were pulled from implementations instead of interfaces.
- Fix issue with autocomplete then punned props are used in JSX. E.g. `<M foo ...>`.
- Fix issue with JSX autocompletion not working after `foo=#variant`.
- Fix issue in JSX autocompletion where the `key` label would always appear.
- Fix issue in record field autocomplete not working with type aliases.
- Fix issue where autocomplete for local values would not work in the presence of `@react.component` annotations.
- Fix issue where the server would crash on large output produced by the binary command.
- Fix issue where the server would crash when a file has a self cycle.

## 1.1.3

Features:

- Find references to files as modules.
- Autocomplete: skip inline comments to decide if a labeled argument was already supplied.
- Rename: support file rename when renaming a module name.
- Rename: use renameProvider to give a warning when it's not a symbol that can be renamed.
- Jump to definition: support jumping to type definition.
- Jump to definition: jump to the `res` file when both `res` and `resi` are present.
- Restore creation of interface files (fully supported from compiler 9.1.3 onwards).

## 1.1.2

Features:

- Rename has landed! Works across-files!
- More autocomplete improvements.
- Wider Linux support.

## 1.1.1

This update contains _lots_ of autocomplete, hover and jump-to-definition improvements. We'll list only a few below.

Fixes:

- Jump-to-definition on some Windows paths.
- `->` autocomplete overruled `.`.
- Hover on components in interface files.

Features:

- Show References! Works cross-files too.
- Hover now supports markdown docs.
- Hover on labels in component functions with compiler version 9.1, and labels with type annotation.
- Don't show file path on hover and autocomplete (cleaner).
- Autocomplete for props in JSX components.
- `->` autocomplete for built-in list, array, string, option types. And for string and array literals.
- Slimmer download.

Breakages:

- Very old linux versions are no longer supported.
- Hover: no more odoc format support (but it'll still display as text).

## 1.0.8

Fixes:

- Diagnostics display for long lines.

Features:

- Full support for the newest `rescript` npm package!
- Highlight type parameters.

## 1.0.7

Fixes:

- Highlighting for some decorators and keywords.
- Various hover & autocomplete opportunities.

Features:

- Autocomplete for `->` pipe!
- Autocomplete for decorators such as `@module` and `@val` and `@deprecated`.
- Autocomplete for labels `func(~...)`.
- Support for the upcoming `rescript` npm package.

## 1.0.6

Fixes:

- Diagnostics crashing when a file's range isn't found (advice: use fewer ppxes that cause these bugs!). See [#77](https://github.com/rescript-lang/rescript-vscode/issues/77).
- Weird behaviors when project path contains white space.
- Proper audit of the windows bugs. Windows is now officially first-class!

Syntax colors:

- Highlight operators for default VSCode dark+ theme. This means slightly less diverse highlight for the other themes that previously already highlighted operators.
- Worked with [One Dark Pro](https://marketplace.visualstudio.com/items?itemName=zhuangtongfa.Material-theme) and [Mariana Pro](https://marketplace.visualstudio.com/items?itemName=rickynormandeau.mariana-pro). We now officially recommend these 2 themes, in addition to the existing recommendations in README.
- Highlight deprecated elements using the deprecation scopes.
- JSX bracket highlight fix (still no color; before, some parts were erroneously highlighted).

## 1.0.5

Features:

- Custom folding. See README.
- Support for doc strings when hovering on modules.
- Jump to type definition for types defined in inner modules.

Fixes:

- Properly highlight nested comments.
- Windows diagnostics!
- Removed a potential infinite loop issue in autocomplete.
- Don't autocomplete `open MyModule` inside line comments.
- Don't print parentheses as in `A()` for 0-ary variants.

## 1.0.4

- Some diagnostics watcher staleness fix.
- Various type hover fixes.
- Monorepo/yarn workspace support.

## 1.0.2

- All the usual features (type hint, autocomplete) now work on `bsconfig.json` too!
- Snippets, to ease a few syntaxes.
- Improved highlighting for polymorphic variants. Don't abuse them please.

## 1.0.1

- Fix temp file creation logic.

## 1.0.0

Official first release!
