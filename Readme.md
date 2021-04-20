# Rescript Editor Support

This is a private command line binary used by [rescript-vscode](https://github.com/rescript-lang/rescript-vscode) to power a few functionalities such as jump to definition, hover and autocomplete.

The binary reads the `.cmt` and `.cmti` files and analyses them.

## Install

```
opam switch 4.06.1
```

## Build

```
make
```

The built artifact is in `lib/rescript-editor-support.exe`

## Usage

Run:

```sh
lib/rescript-editor-support.exe --help
```

## History

This project is based on a fork of [Reason Language Server](https://github.com/jaredly/reason-language-server).

Distributed under the MIT License (see [LICENSE](./LICENSE)).
