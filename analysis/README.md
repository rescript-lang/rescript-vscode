# Analysis Library and Binary

This subfolder builds a private command line binary used by the plugin to power a few functionalities such as jump to definition, hover and autocomplete.

The binary reads the `.cmt` and `.cmti` files and analyses them.

For installation & build instructions, see the main CONTRIBUTING.md.

## Overview

See main CONTRIBUTING.md's repo structure. Additionally, `examples/` is a convenience debugging repo. Check out `test.sh` (invoked through `make test`) to see the snapshots testing workflow stored in `tests/`.

## Usage

At root:
```sh
./rescript-editor-analysis.exe --help

# or

dune exec -- rescript-editor-analysis --help
```

## History

This project is based on a fork of [Reason Language Server](https://github.com/jaredly/reason-language-server).

## Tests

The tests in the `analysis/test` folder are based on the `./rescript-editor-analysis.exe test` command. This special subcommand processes a file and executes specific editor analysis functionality based on special syntax found in code comments.

Consider the following code:

```res
let a = 5
// a.
//   ^com
```

After building the ReScript project (**⚠️ this is a requirement**), you can execute `./rescript-editor-analysis.exe test Sample.res`, and completion will be executed for the cursor position indicated by `^`. The `com` directive requests completion. To see other commands, check out the pattern match in `test` in [Commands.ml](./src/Commands.ml).

Here’s how it works: once a command is found in a comment, a copy of the source file is created inside a temporary directory, where the line above `^com` is uncommented. The corresponding analysis functionality is then processed, typically with `~debug:true`. With debug enabled, code paths like

```ml
if Debug.verbose () then
      print_endline "[complete_typed_value]--> Tfunction #other";
```

will print to stdout. This is helpful for observing what occurs during the analysis.

When you run `make test` (from the `analysis/tests` folder), `./rescript-editor-analysis.exe test <file>` will be executed for each `*.res` file in `analysis/tests/src`. The stdout will be compared to the corresponding `analysis/tests/src/expected` file. If `git diff` indicates changes, `make test` will fail, as these differences might be unintentional.
