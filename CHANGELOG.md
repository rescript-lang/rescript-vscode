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

- Add autocomplete for function argument values (booleans, variants and options. More values coming), both labelled and unlabelled. https://github.com/rescript-lang/rescript-vscode/pull/665
- Add autocomplete for JSX prop values. https://github.com/rescript-lang/rescript-vscode/pull/667
- Add snippet support in completion items. https://github.com/rescript-lang/rescript-vscode/pull/668
- Add support from completing polyvariants as values. https://github.com/rescript-lang/rescript-vscode/pull/669
- Add support for completion in patterns. https://github.com/rescript-lang/rescript-vscode/pull/670
- Add support for pattern completion of unsaved tuples. https://github.com/rescript-lang/rescript-vscode/pull/679
- Add support for completion in typed expressions. https://github.com/rescript-lang/rescript-vscode/pull/682
- Complete for `React.element` creator functions (`React.string` etc) when in JSX context. https://github.com/rescript-lang/rescript-vscode/pull/681
- Handle optional record fields in expression/pattern completion. https://github.com/rescript-lang/rescript-vscode/pull/691
- Expand options in completion to make working with options a bit more ergonomic. https://github.com/rescript-lang/rescript-vscode/pull/690
- Let `_` trigger completion in patterns. https://github.com/rescript-lang/rescript-vscode/pull/692
- Support inline records in completion. https://github.com/rescript-lang/rescript-vscode/pull/695
- Add way to autocomplete an exhaustive switch statement for identifiers. Example: an identifier that's a variant can have a switch autoinserted matching all variant cases. https://github.com/rescript-lang/rescript-vscode/pull/699
- Support typed expression completion for lowercase (builtin) JSX tags. https://github.com/rescript-lang/rescript-vscode/pull/702
- Support typed expression completion driven by type annotations. https://github.com/rescript-lang/rescript-vscode/pull/711

#### :nail_care: Polish

- Prefer opened `Belt` modules in autocomplete when `-open Belt` is detected in `bsconfig`. https://github.com/rescript-lang/rescript-vscode/pull/673
- Improve precision in signature help. You now do not need to type anything into the argument for it to highlight. https://github.com/rescript-lang/rescript-vscode/pull/675
- Remove redundant function name in signature help, to clean up what's shown to the user some. https://github.com/rescript-lang/rescript-vscode/pull/678
- Show docstrings in hover for record fields and variant constructors. https://github.com/rescript-lang/rescript-vscode/pull/694
- The necessary leading `?` is now automatically inserted for optional fields when destructuring records. https://github.com/rescript-lang/rescript-vscode/pull/715

#### :bug: Bug Fix

- Highlight `catch` like a keyword https://github.com/rescript-lang/rescript-vscode/pull/677
- Make signature help work in calls nested inside of other calls. https://github.com/rescript-lang/rescript-vscode/pull/687
- Fix pipe completion to work on aliased types. https://github.com/rescript-lang/rescript-vscode/pull/700
- Fix issue with not finding type for function arguments. https://github.com/rescript-lang/rescript-vscode/pull/706
- Fix incorrect syntax in hover help for module. https://github.com/rescript-lang/rescript-vscode/pull/709

## v1.10.0

#### :nail_care: Polish

- Remove spacing between type definition in clients that do not support markdown links. https://github.com/rescript-lang/rescript-vscode/pull/619
- Rename custom LSP methods names. https://github.com/rescript-lang/rescript-vscode/pull/611
- Better performance for Inlay Hints and Codelens. https://github.com/rescript-lang/rescript-vscode/pull/634
- Accept both `@ns.doc` and the new `@res.doc` for the internal representation of doc comments. And both `@ns.optional` and `@res.optional` for the optional fields. https://github.com/rescript-lang/rescript-vscode/pull/642
- Make pipe completion work more reliably after function calls. https://github.com/rescript-lang/rescript-vscode/pull/656
- Make pipe completion work in pipe chains, not just on the first pipe. https://github.com/rescript-lang/rescript-vscode/pull/656
- Make pipe completion work reliably when the path resolution needs to traverse submodules https://github.com/rescript-lang/rescript-vscode/pull/663
- Make pipe completion work (after saving/compiling) when the return type of a function call is unknown until compilation https://github.com/rescript-lang/rescript-vscode/pull/662
- Add pipe completion for `int` and `float` constants https://github.com/rescript-lang/rescript-vscode/pull/664

#### :bug: Bug Fix

- Fix issue where `-open Some.Path` in `"bsc-flags"` would sometimes be treated differently from `open Some.Path` locally in a file https://github.com/rescript-lang/rescript-vscode/pull/616

- Fix issue where doc comment is not shown on hover in case of shadowed identifier (in particular for JSX V4 components which shadow `make`) https://github.com/rescript-lang/rescript-vscode/issues/621

- Adapt command to create interface files to latest JSX V4 (no key prop, possibly empty record) https://github.com/rescript-lang/rescript-vscode/issues/617

- Fix issue where pipes were not taken into account in the signature help, resulting in the highlighted argument in signature help always being off by one for unlabelled arguments in piped expressions https://github.com/rescript-lang/rescript-vscode/issues/626

- Fix incorrect type hint for module type. https://github.com/rescript-lang/rescript-vscode/pull/626

- Fix file location in Document Symbols response. https://github.com/rescript-lang/rescript-vscode/issues/629

- Fix issue where create interface file would not work with certain JSX V4 components https://github.com/rescript-lang/rescript-vscode/issues/617

- Fix issue with completing `foo. x` where `x` should not be part of the completion https://github.com/rescript-lang/rescript-vscode/pull/644

- Fix issue where code analysis would not track types in inner modules across implementations and interfaces https://github.com/rescript-association/reanalyze/issues/186

- Fix issue with references to elements defined in an interface file https://github.com/rescript-lang/rescript-vscode/pull/646

- Fix issue with references from implementation files which also happen to have interface files https://github.com/rescript-lang/rescript-vscode/issues/645

- Fix issue where jump to definition would go to the wrong place when there are aliased identifiers in submodules https://github.com/rescript-lang/rescript-vscode/pull/653

- Fix issue where document symbols were not shown nested https://github.com/rescript-lang/rescript-vscode/pull/655

## v1.8.2

#### :rocket: New Feature

- Add configuration parameter `"transitive"` under `"reanalyze"` is `bsconfig.json` and make reportst non-transitive by default. If set to `false`, the analysis does not report transitively dead items. So removing the reported item individually can be done in isolation. This is a more fine-grained process for guiding the user to remove dead code one item at a time. https://github.com/rescript-lang/rescript-vscode/pull/601 https://github.com/rescript-lang/rescript-vscode/pull/610
  This feature comes from a conversation with @jfmengels on how https://github.com/jfmengels/elm-review is designed.

#### :bug: Bug Fix

- Fix issue where module paths in `-open` in `bsc-flags` such as "-open ReScriptJs.Js" were not recognized https://github.com/rescript-lang/rescript-vscode/issues/607

## v1.8.1

#### :rocket: New Feature

- Add support for prop completion for JSX V4 https://github.com/rescript-lang/rescript-vscode/pull/579
- Add support for create interface file for JSX V4 https://github.com/rescript-lang/rescript-vscode/pull/580
- Expand one level of type definition on hover. Dig into record/variant body. https://github.com/rescript-lang/rescript-vscode/pull/584
- Add clickable links to type definitions in hovers. https://github.com/rescript-lang/rescript-vscode/pull/585
- Add experimental signature help for function calls. https://github.com/rescript-lang/rescript-vscode/pull/547

#### :bug: Bug Fix

- Fix printing of record types with optional fields https://github.com/rescript-lang/rescript-vscode/pull/584

## v1.6.0

#### :rocket: New Feature

- Inlay Hints (experimental). `rescript.settings.inlayHints.enable: true`. Turned off by default. https://github.com/rescript-lang/rescript-vscode/pull/453
- Code Lenses for functions (experimental). `rescript.settings.codeLens: true`. Turned off by default. https://github.com/rescript-lang/rescript-vscode/pull/513
- Markdown code blocks tagged as `rescript` now get basic syntax highlighting. https://github.com/rescript-lang/rescript-vscode/pull/97
- Hover support for doc comments on v10 compiler `/** this is a doc comment */`

#### :bug: Bug Fix

- Fix issue where debug output would end up in the JSON file produced by Reanalyze https://github.com/rescript-lang/rescript-vscode/pull/575
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
