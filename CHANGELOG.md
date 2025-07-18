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

- Find `bsc.exe` and `rescript-code-editor-analysis.exe` from platform-specific packages used by ReScript `v12.0.0-alpha.13`+.https://github.com/rescript-lang/rescript-vscode/pull/1092
- Find `rewatch.exe` from platform-specific packages used by ReScript `v12.0.0-alpha.13`+. https://github.com/rescript-lang/rescript-vscode/pull/1101

#### :bug: Bug fix

- Fix: show existing compiler errors and warnings on file open. https://github.com/rescript-lang/rescript-vscode/pull/1103

- Fix: bug where we incorrectly showed a warning notification about something going wrong with incremental type checking, when in fact the compiler was reporting module-related type errors https://github.com/rescript-lang/rescript-vscode/pull/1090

- Fix: bug where we incorrectly showed a warning notification about something going wrong with incremental type checking, when in fact the compiler was reporting multiple definitions of the same type or module name https://github.com/rescript-lang/rescript-vscode/pull/1086

- Fix: incorrect highlighting of `as` inside labelled arguments like `toast` https://github.com/rescript-lang/rescript-vscode/pull/1085

- Fix: bug where incremental analysis does not work when the project folder contains a dot. https://github.com/rescript-lang/rescript-vscode/pull/1080

- Fix: bug where incremental compilation crashes when rewatch is being run in a specific package vs the root of the monorepo. https://github.com/rescript-lang/rescript-vscode/pull/1082

- Fix: Absence of Node.js does not hinder LSP server. https://github.com/rescript-lang/rescript-vscode/pull/1083

- Fix: JSON from `rescript-code-editor-analysis` was not always escaped properly, which prevented code actions from being available in certain situations https://github.com/rescript-lang/rescript-vscode/pull/1089

#### :house: Internal

- Find binary paths asynchronously. On `>=12.0.0-alpha.13` we do this by dynamically importing the `@rescript/{target}` package in the project root. https://github.com/rescript-lang/rescript-vscode/pull/1093
- Remove chokidar from LSP server. We expect LSP clients to support [workspace_didChangeWatchedFiles](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeWatchedFiles). https://github.com/rescript-lang/rescript-vscode/pull/1096

## 1.62.0

#### :nail_care: Polish

- Remove the built-in formatter since it has been causing more harm than done good. https://github.com/rescript-lang/rescript-vscode/pull/1073

#### :rocket: New Feature

- Port [7292](https://github.com/rescript-lang/rescript/pull/7292): Fix dot completion issue with React primitives. https://github.com/rescript-lang/rescript-vscode/pull/1074

- Add support for "dot completion everywhere". In addition to record fields, dots will now complete for object fields, and pipe completions applicable to the type the dot is on. You can also configure where the editor draws extra pipe completions from via the `@editor.completeFrom` attribute. https://github.com/rescript-lang/rescript-vscode/pull/1054

#### :bug: Bug fix

- Fix bug where type args stopped working in some completions when passed through inline records. https://github.com/rescript-lang/rescript-vscode/pull/1064
- Fix bug where objects weren't actually iterated on, making completions inside of them not work. https://github.com/rescript-lang/rescript-vscode/pull/1065
- Fix bug where pipe completions would not trigger with generic type arguments. https://github.com/rescript-lang/rescript-vscode/pull/1067

## 1.60.0

#### :rocket: New Feature

- Enable incremental typechecking and project config cache by default. https://github.com/rescript-lang/rescript-vscode/pull/1047

#### :house: Internal

- Auto-format vendored OCaml sources like in compiler repo. https://github.com/rescript-lang/rescript-vscode/pull/1053
- All OCaml sources in this repo is now considered "legacy", as the OCaml parts of the editor integration are now shipped with the compiler instead.

## 1.58.0

#### :bug: Bug fix

- Fix an issue where the extension would still crash in a monorepo with rewatch

#### :rocket: New Feature

- Add hightlighting for the new dict literal syntax `dict{}`. https://github.com/rescript-lang/rescript-vscode/pull/934

## 1.56.0

#### :bug: Bug Fix

- Fix a regression with incremental typechecking in monorepos with rewatch, where the workspace directory was not properly set.
- When log files are deleted (due to a clean), the editor tooling doesn't crash anymore.

#### :rocket: New Feature

- Support for the `namespace-entry` feature of rewatch, to allow entrypoint modules for namespaced packages.

## 1.54.0

#### :nail_care: Polish

- Reduce latency of language server by caching a few project config related things. https://github.com/rescript-lang/rescript-vscode/pull/1003

#### :bug: Bug Fix

- Fix edge case in switch expr completion. https://github.com/rescript-lang/rescript-vscode/pull/1002

## 1.52.0

#### :rocket: New Feature

- Experimental support for caching the project config to reduce latency. https://github.com/rescript-lang/rescript-vscode/pull/1000

#### :bug: Bug Fix

- Fix highlighting of other languages being affected by rescript-vscode. https://github.com/rescript-lang/rescript-vscode/pull/973
- Use canonicalized URIs/paths for jump to definition. https://github.com/rescript-lang/rescript-vscode/pull/982
- Fix JSX prop special case in end of JSX element. https://github.com/rescript-lang/rescript-vscode/pull/984
- preserve URI format in canonicalizeUri. https://github.com/rescript-lang/rescript-vscode/pull/990
- Remove workaround for canonicalize function in tests https://github.com/rescript-lang/rescript-vscode/pull/992
- Get completions for writing new field names in a record body expressions in more cases. https://github.com/rescript-lang/rescript-vscode/pull/997

#### :nail_care: Polish

- Make sure doc strings are always on top in hovers. https://github.com/rescript-lang/rescript-vscode/pull/956
- Make JSX completion work for `make` functions of type `React.component<props>`, like what you get when using `React.lazy_`. https://github.com/rescript-lang/rescript-vscode/pull/966
- Hover: print signature above docstrings. https://github.com/rescript-lang/rescript-vscode/pull/969
- Adjust function template snippet return. https://github.com/rescript-lang/rescript-vscode/pull/985
- Don't expand `type t` maker functions in patterns. https://github.com/rescript-lang/rescript-vscode/pull/986
- Use `loc` for identifiers to get more and better completions in certain scenarios with type parameters. https://github.com/rescript-lang/rescript-vscode/pull/993
- Improve the DX of running the code analyzer some. https://github.com/rescript-lang/rescript-vscode/pull/995

#### :rocket: New Feature

- Add support for the rewatch build system for incremental compilation. https://github.com/rescript-lang/rescript-vscode/pull/965
- Add support for Linux ARM64
- Statically linked Linux binaries
- Emit `%todo` instead of `failwith("TODO")` when we can (ReScript >= v11.1). https://github.com/rescript-lang/rescript-vscode/pull/981
- Complete `%todo`. https://github.com/rescript-lang/rescript-vscode/pull/981
- Add code action for extracting a locally defined module into its own file. https://github.com/rescript-lang/rescript-vscode/pull/983
- Add code action for expanding catch-all patterns. https://github.com/rescript-lang/rescript-vscode/pull/987
- Add code actions for removing unused code (per item and for an entire file), driven by `reanalyze`. https://github.com/rescript-lang/rescript-vscode/pull/989

#### :house: Internal

- Update parser and compiler support files to the latest version. https://github.com/rescript-lang/rescript-vscode/pull/998

## 1.50.0

#### :rocket: New Feature

- Extend signature help to work on constructor payloads in both expressions and patterns as well. Can be turned off if wanted through settings. https://github.com/rescript-lang/rescript-vscode/pull/947 https://github.com/rescript-lang/rescript-vscode/pull/954
- Show module docs for file modules. https://github.com/rescript-lang/rescript-vscode/pull/952

#### :nail_care: Polish

- Enhance variant constructor payload completion. https://github.com/rescript-lang/rescript-vscode/pull/946
- Clean occasional dots from "insert missing fields" code action. https://github.com/rescript-lang/rescript-vscode/pull/948
- Pick up code actions in incremental compilation. https://github.com/rescript-lang/rescript-vscode/pull/948
- Various improvements to the signature help functionality. https://github.com/rescript-lang/rescript-vscode/pull/950
- Clean up completion item "details" and "documentation". https://github.com/rescript-lang/rescript-vscode/pull/952

## 1.48.0

#### :bug: Bug Fix

- Stability fixes for the experimental incremental compilation mode. https://github.com/rescript-lang/rescript-vscode/pull/945

## 1.46.0

#### :bug: Bug Fix

- Fix null checks for editor config, so things don't blow up. https://github.com/rescript-lang/rescript-vscode/pull/944

## 1.44.0

#### :rocket: New Feature

- Experimental support for type checking without saving the file :tada:. https://github.com/rescript-lang/rescript-vscode/pull/939

## 1.42.0

#### :bug: Bug Fix

- Fix issue with unlabelled arg code swallowing completions. https://github.com/rescript-lang/rescript-vscode/pull/937
- Fix issue where completion inside of switch expression would not work in some cases. https://github.com/rescript-lang/rescript-vscode/pull/936
- Fix bug that made empty prop expressions in JSX not complete if in the middle of a JSX element. https://github.com/rescript-lang/rescript-vscode/pull/935

## 1.40.0

#### :nail_care: Polish

- Enhance decorator completion. https://github.com/rescript-lang/rescript-vscode/pull/908
- Completion for import attributes in `@module`. https://github.com/rescript-lang/rescript-vscode/pull/913
- Relax filter for what local files that come up in from and regular string completion in `@module`. https://github.com/rescript-lang/rescript-vscode/pull/918
- Make from completion trigger for expr hole so we get a nice experience when completing {from: <com>} in `@module`. https://github.com/rescript-lang/rescript-vscode/pull/918
- Latest parser for newest syntax features. https://github.com/rescript-lang/rescript-vscode/pull/917
- Handle completion for DOM/element attributes and attribute values properly when using a generic JSX transform. https://github.com/rescript-lang/rescript-vscode/pull/919
- Highlight tagged template literal functions as functions. https://github.com/rescript-lang/rescript-vscode/pull/920
- Complete for `type t` values when encountering a `type t` in relevant scenarios. https://github.com/rescript-lang/rescript-vscode/pull/924
- Highlight escaped sequences as a whole and not only the first character. https://github.com/rescript-lang/rescript-vscode/pull/929
- Start highlighting escaped sequences in template literals. https://github.com/rescript-lang/rescript-vscode/pull/929

## 1.38.0

#### :nail_care: Polish

- Prefer Core's `RegExp` when Core is open and completing for regexp functions. https://github.com/rescript-lang/rescript-vscode/pull/903
- Add `%re("")` to the completions list when completing in a position where a regexp value is expected. https://github.com/rescript-lang/rescript-vscode/pull/903

#### :bug: Bug Fix

- Fix issue with completion in nested patterns that would make it not possible to complete for new record fields via trailing commas in certain situations. https://github.com/rescript-lang/rescript-vscode/pull/906

## 1.36.0

#### :bug: Bug Fix

- Fix issue with ambigious wraps in JSX prop values (`<SomeComp someProp={<com>}`) - need to figure out if we're completing for a record body or if `{}` are just wraps for the type of `someProp`. In the case of ambiguity, completions for both scenarios are provided. https://github.com/rescript-lang/rescript-vscode/pull/894
- Many bugfixes around nested pattern and expression completion. https://github.com/rescript-lang/rescript-vscode/pull/892
- Fix (very annoying) issue where empty pipe completion wouldn't work inside of a parenthesised function call: `Console.log(someArray->)` completing at the pipe. https://github.com/rescript-lang/rescript-vscode/pull/895

#### :nail_care: Polish

- More cases of not emitting `_` when completing in expressions. https://github.com/rescript-lang/rescript-vscode/pull/890

#### :house: Internal

- Move `rescript-tools` to OCaml code and make `analysis` an library. https://github.com/rescript-lang/rescript-vscode/pull/855

## 1.34.0

#### :rocket: New Feature

- Complete domProps for lowercase JSX components from `ReactDOM.domProps` if possible. https://github.com/rescript-lang/rescript-vscode/pull/883
- Complete for maker-style functions (functions returning type `t` of a module) when encountering a `type t` in relevant scenarios. https://github.com/rescript-lang/rescript-vscode/pull/884
- Expand type aliases in hovers. https://github.com/rescript-lang/rescript-vscode/pull/881

#### :nail_care: Polish

- Better error recovery when analysis fails. https://github.com/rescript-lang/rescript-vscode/pull/880
- Do not emit `_` when completing in expressions. https://github.com/rescript-lang/rescript-vscode/pull/885
- Include fields when completing a braced expr that's an ID, where it the path likely starts with a module. https://github.com/rescript-lang/rescript-vscode/pull/882

## 1.32.0

#### :bug: Bug Fix

- Fix so that you don't need a leading `#` to complete for polyvariant constructors. https://github.com/rescript-lang/rescript-vscode/pull/874
- Print keyword polyvariant constructors with quotes when doing completions. https://github.com/rescript-lang/rescript-vscode/pull/877

## 1.30.0

#### :rocket: New Feature

- If interface file exists, ask if it should be overwritten. https://github.com/rescript-lang/rescript-vscode/pull/865

#### :bug: Bug Fix

- Proper default for `"uncurried"` in V11 projects. https://github.com/rescript-lang/rescript-vscode/pull/867
- Treat `result` type as a proper built in type. https://github.com/rescript-lang/rescript-vscode/pull/860
- Fix infinite loop when resolving inferred completions when several values in scope has the same name. https://github.com/rescript-lang/rescript-vscode/pull/869
- Fix crash when trying to print recursive polymorphic variants without a concrete definition. https://github.com/rescript-lang/rescript-vscode/pull/851
- Fix `rescript-language-server --version` command. https://github.com/rescript-lang/rescript-vscode/pull/873
- Print exotic polyvariant constructor names with quotes when doing completion. https://github.com/rescript-lang/rescript-vscode/pull/870

#### :nail_care: Polish

- Change end position of cursor when completing `Some(<fieldName>)` in patterns. https://github.com/rescript-lang/rescript-vscode/pull/857

#### :bug: Bug Fix

- Add support for detecting dead fields inside inline records. https://github.com/rescript-lang/rescript-vscode/pull/858

## 1.28.0

#### :bug: Bug Fix

- Fix issue introduced in recent PR for module completion. https://github.com/rescript-lang/rescript-vscode/pull/856

## 1.26.0

#### :bug: Bug Fix

- More robust handling of namespaces in pipe completions. https://github.com/rescript-lang/rescript-vscode/pull/850

## 1.24.0

#### :bug: Bug Fix

- Clean up name of namespaced module when hovering. https://github.com/rescript-lang/rescript-vscode/pull/845
- Don't complete illegal file module names. https://github.com/rescript-lang/rescript-vscode/pull/844
- Fix issue `open` on submodules exposed via `-open` in bsconfig.json/rescript.json, that would cause the content of those `open` modules to not actually appear in autocomplete. https://github.com/rescript-lang/rescript-vscode/pull/842
- Account for namespace when filtering pipe completion items. https://github.com/rescript-lang/rescript-vscode/pull/843

## 1.22.0

#### :nail_care: Polish

- Resolve module aliases in hover. https://github.com/rescript-lang/rescript-vscode/pull/820

## 1.20.0

#### :rocket: New Feature

- Add support for syntax highlighting in `%raw` and `%ffi` extension points. https://github.com/rescript-lang/rescript-vscode/pull/774
- Add completion to top level decorators. https://github.com/rescript-lang/rescript-vscode/pull/799
- Add code action for wrapping patterns where option is expected with `Some`. https://github.com/rescript-lang/rescript-vscode/pull/806
- Better completion from identifiers with inferred types. https://github.com/rescript-lang/rescript-vscode/pull/808
- Make suggested template functions async when the target function returns a promise. https://github.com/rescript-lang/rescript-vscode/pull/816
- Fix code action for inserting undefined record fields in ReScript v11. https://github.com/rescript-lang/rescript-vscode/pull/817

#### :nail_care: Polish

- Revamp "Insert missing cases" code action to make it apply in more cases and be much more robust. https://github.com/rescript-lang/rescript-vscode/pull/804
- Make the completion engine understand async/await. https://github.com/rescript-lang/rescript-vscode/pull/813
- Comments are now automatically closed and indented. https://github.com/rescript-lang/rescript-vscode/pull/815

#### :bug: Bug Fix

- Fix invalid range for `definition`. https://github.com/rescript-lang/rescript-vscode/pull/781
- Don't emit object keys in uppercase as namespace. https://github.com/rescript-lang/rescript-vscode/pull/798
- Fix accidental output of extra `|` when producing exhaustive switch code for polyvariants. https://github.com/rescript-lang/rescript-vscode/pull/805
- Fix JS syntax highlighting in single-line FFI extension points. https://github.com/rescript-lang/rescript-vscode/pull/807
- Fix signature help in uncurried mode. https://github.com/rescript-lang/rescript-vscode/pull/809
- Fix various issues in uncurried mode. https://github.com/rescript-lang/rescript-vscode/pull/810
- Fixes a bug in pattern completion where for example `result` wouldn't complete, due to type variables getting lost/not being instantiated. https://github.com/rescript-lang/rescript-vscode/pull/814
- Fix bug where pipes would not be considered in certain cases when completing for single unlabelled function arguments. https://github.com/rescript-lang/rescript-vscode/pull/818

## 1.18.0

#### :rocket: New Feature

- Docstring template Code Action. https://github.com/rescript-lang/rescript-vscode/pull/764
- Improve unlabelled argument names in completion function templates. https://github.com/rescript-lang/rescript-vscode/pull/754
- Add `Some(fieldName)` case when completing in a pattern with an option on a record field. https://github.com/rescript-lang/rescript-vscode/pull/766

#### :bug: Bug Fix

- Fix URL scheme for Code Action. https://github.com/rescript-lang/rescript-vscode/pull/748
- Support uncurried functions in various places where we look up and use function types. https://github.com/rescript-lang/rescript-vscode/pull/771

## 1.16.0

#### :rocket: New Feature

- Greatly extend completion abilities for unsaved code. WARNING: Might be a bit unstable initially. Report any issues you see. https://github.com/rescript-lang/rescript-vscode/pull/712
- Provide hovers for more unsaved code via the new completion features. https://github.com/rescript-lang/rescript-vscode/pull/749

## 1.14.0

#### :rocket: New Feature

- Enable completion for `Js.Exn.Error(error)` when pattern matching on `exn`. This is to make the `Js.Exn.Error` API more discoverable. https://github.com/rescript-lang/rescript-vscode/pull/728

#### :nail_care: Polish

- Signature Help is now considered stable, and enabled for all users. Can still be turned off in settings.
- Show whether record fields and variant constructors are deprecated when completing. https://github.com/rescript-lang/rescript-vscode/pull/731
- Prettify how optional record fields are printed in the completion item detail. https://github.com/rescript-lang/rescript-vscode/pull/737

#### :bug: Bug Fix

- Fix crashes in document symbol requests when broken syntax exists. https://github.com/rescript-lang/rescript-vscode/pull/736

## 1.12.0

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
- Completion for lowercase JSX elements, treating them like HTML elements. https://github.com/rescript-lang/rescript-vscode/pull/719

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
