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

- Expose more `decode` functions. https://github.com/rescript-lang/rescript-vscode/pull/866

#### :house: [Internal]

- Add env var `FROM_COMPILER` to extract docstrings from compiler repo. https://github.com/rescript-lang/rescript-vscode/pull/868

#### :bug: Bug Fix

- Fix tagged variant for `Module` and add attr to interface files. https://github.com/rescript-lang/rescript-vscode/pull/866
- Fix `rescript-tools --version` command. https://github.com/rescript-lang/rescript-vscode/pull/853
- Fix output truncate when run `rescript-tools doc path/to/file.res` in a separate process. https://github.com/rescript-lang/rescript-vscode/pull/868
