# AGENTS.md

This file provides guidance to AI coding assistants when working with code in this repository.

## Project Overview

This is the official ReScript VSCode extension, providing language support for ReScript (.res/.resi files) in Visual Studio Code. The project uses a Language Server Protocol (LSP) architecture with a TypeScript client/server and native OCaml binaries for analysis.

## Architecture

### Key Components

- **client/**: VSCode extension client (`client/src/extension.ts`) - handles UI, commands, and language client initialization
- **server/**: Language Server (`server/src/server.ts`, `server/src/cli.ts`) - implements LSP features, communicates with ReScript compiler
- **analysis/**: Native OCaml binary for code analysis, hover, autocomplete, and other language features. This is for older ReScript versions only (ReScript 11 and below). New features are usually only implemented in the rescript compiler monorepo.
- **tools/**: ReScript tools binary for additional functionality like interface file generation. This is also for older ReScript versions only (ReScript 11 and below). New features are usually only implemented in the rescript compiler monorepo.
- **grammars/**: TextMate grammar files for syntax highlighting
- **snippets.json**: Code snippets for common ReScript patterns

### Build System

The project uses:

- **dune**: For building OCaml components (analysis & tools)
- **esbuild**: For bundling TypeScript client/server
- **npm**: For JavaScript/TypeScript dependencies
- **TypeScript**: For type checking the client/server code

## Development Commands

### Initial Setup

```bash
npm install                    # Install all dependencies including client/server
opam switch 5.2.0             # Install OCaml switch (if not already done)
opam install . --deps-only    # Install OCaml dependencies
```

### Building

```bash
make build                    # Build OCaml binaries and copy to root
npm run compile               # Compile TypeScript (client & server)
npm run bundle                # Bundle for production (esbuild)
npm run vscode:prepublish     # Clean and bundle (used for publishing)
```

### Development

```bash
npm run watch                 # Watch TypeScript compilation
make -C analysis test         # Run analysis tests
make -C tools/tests test      # Run tools tests
make test                     # Run all tests
```

### Code Quality

```bash
make format                   # Format OCaml (dune) and JS/TS (prettier)
make checkformat              # Check formatting without modifying
make clean                    # Clean build artifacts
```

### Running the Extension in Development

1. Open the project in VSCode
2. Press F5 to launch a new VSCode window (Extension Development Host)
3. Open a ReScript project to test the extension

## Key Files

### Configuration

- `package.json`: Extension manifest, commands, settings, and scripts
- `rescript.configuration.json`: Editor configuration for ReScript files
- `client/src/extension.ts`: Extension entry point and client initialization
- `server/src/server.ts`: Language server implementation
- `server/src/cli.ts`: CLI entry point for the language server

### OCaml Components

- `analysis/`: Code analysis binary (hover, autocomplete, etc.)
- `tools/`: ReScript tools binary (interface generation, etc.)

### Language Features

- **LSP Features**: hover, goto definition, find references, rename, autocomplete
- **Code Analysis**: dead code detection, exception analysis (via reanalyze)
- **Build Integration**: compile diagnostics, status indicators
- **Commands**: interface creation, file switching, compiled JS opening

## Testing

The project has several test suites:

- `analysis/tests/`: Tests for the analysis binary
- `tools/tests/`: Tests for ReScript tools
- `analysis/tests-incremental-typechecking/`: Incremental typechecking tests
- `analysis/tests-generic-jsx-transform/`: JSX transformation tests

## Project Structure Notes

- The extension supports both `.res` (implementation) and `.resi` (interface) files
- Uses VSCode Language Client protocol for communication between client and server
- Native binaries are cross-platform (darwin, linux, win32) and included in the extension
- Supports workspace configurations and monorepo structures
- Incremental type checking can be enabled for better performance on large projects
- As mentioned above the native OCaml binaries here are only here for backwards-compatibility with ReScript versions 11 or below. Since ReScript 12 both `analysis` and `tools` are part of the ReScript compiler monorepo, thus refrain from changing them too much (bugfixes that need to be ported are ok).
