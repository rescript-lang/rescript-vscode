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

## 0.6.0

#### :rocket: New Feature

- _internal_ Add experimental command for extracting (string) contents from extension points.

## 0.5.0

#### :rocket: New Feature

- Add `source` property to type, value, module and module alias. https://github.com/rescript-lang/rescript-vscode/pull/900.

#### :bug: Bug Fix

- Print docstrings for nested submodules. https://github.com/rescript-lang/rescript-vscode/pull/897
- Print `deprecated` field for module. https://github.com/rescript-lang/rescript-vscode/pull/897

## 0.4.0

#### :bug: Bug Fix

- Support inline record fields in constructors. https://github.com/rescript-lang/rescript-vscode/pull/889
- Fix docstrings for module alias. Get internal docstrings of module file. https://github.com/rescript-lang/rescript-vscode/pull/878
- Fix extracted docs of types include escaped linebreaks in signature. https://github.com/rescript-lang/rescript-vscode/pull/891

## 0.3.0

#### :rocket: New Feature

- Expose more `decode` functions. https://github.com/rescript-lang/rescript-vscode/pull/866

#### :house: [Internal]

- Add env var `FROM_COMPILER` to extract docstrings from compiler repo. https://github.com/rescript-lang/rescript-vscode/pull/868

#### :bug: Bug Fix

- Fix tagged variant for `Module` and add attr to interface files. https://github.com/rescript-lang/rescript-vscode/pull/866
- Fix `rescript-tools --version` command. https://github.com/rescript-lang/rescript-vscode/pull/873
- Fix output truncate when run `rescript-tools doc path/to/file.res` in a separate process. https://github.com/rescript-lang/rescript-vscode/pull/868
