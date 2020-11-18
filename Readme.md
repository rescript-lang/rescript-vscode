
# Rescript Editor Support

Command-line for reading compiler artifacts (`.cmt` and `.cmti` files) and support jump to definition, hover, and autocomplete. This is intended to be used in higher level language servers.

## Status

This is a preliminary version for early testing, the CLI is not stable yet.

## Build

```
dune build
```

or

```
esy
```


## Usage

### dump

Compute jump-to-definition and hover information for `Foo.res` at line `0` and column `4`:

```
rescript-editor-support.exe dump src/Foo.res:0:4
```

### complete

Compute autocomplete for `Foo.res` at line `0` and column `4`, where `Foo.res` is being edited and its current content in the editor is in file `Current.res`.

```
rescript-editor-support.exe complete src/Foo.res:0:4 Current.res
```


## History

This project is based on a fork of [Reason Language Server](https://github.com/jaredly/reason-language-server).

Distributed under the MIT License (see [LICENSE](./LICENSE)).
