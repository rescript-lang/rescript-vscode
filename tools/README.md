# ReScript Tools

## Install

```sh
npm install --save-dev @rescript/tools
```

## CLI Usage

```sh
restools --help
```

### Generate documentation

Print JSON:

```sh
restools doc src/EntryPointLibFile.res
```

Write JSON:

```sh
restools doc src/EntryPointLibFile.res > doc.json
```

### Reanalyze

```sh
restools reanalyze --help
```

## Decode JSON

Add to `bs-dev-dependencies`:

```json
"bs-dev-dependencies": ["@rescript/tools"]
```

```rescript
// Read JSON file and parse with `Js.Json.parseExn`
json->RescriptTools.Docgen.decodeFromJson
```
