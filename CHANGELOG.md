## 1.0.x (unreleased)
- Fix diagnostics when location's not found (advice: use fewer ppxes!). See [#77](https://github.com/rescript-lang/rescript-vscode/issues/77).

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
