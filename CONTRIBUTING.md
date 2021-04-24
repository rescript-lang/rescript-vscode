# Contributing

Thanks for your interest. Below is an informal spec of how the plugin's server communicates with the actual compiler. If you're a ReScript editor plugin implementor, you should probably read this to understand the various important nuances and copy it.

## Other Editors With Language-Server Support

This repo happens to also contain a language-server usable by other editors. If you'd like to use this language-server with e.g. Atom, for now, you have to clone the repo and run `npm run compile`. The language server will be at `server/out/server.js`. Wire that into your editor.

## Repo Structure

```
.
├── client // Language Client
│   └── src
│       └── extension.ts // Language Client entry point
├── package.json // The extension manifest.
└── server // Language Server
    ├── src
    │   └── server.ts // Language Server entry point
    ├── win32
    ├── linux
    └── darwin // these 3 folders contain rescript-editor-support.exe
```

## Run the Project

- Run `npm install` at the root. This will also install the necessary npm modules in both the `client` and `server` folders.
- Open VS Code to this folder.
- Switch to the Debug viewlet (command palette -> View: Show Run and Debug).
- Select `Client + Server` from the drop down, launch it (green arrow):

  <img width="235" alt="image" src="https://user-images.githubusercontent.com/1909539/97448097-7d186a80-18ed-11eb-82d6-d55b70f54811.png">

  If you're getting some Promise-related error alert: this is a VSCode and/or template bug.
  - If that newly launched VSCode test instance has no project in its explorer view, drag in a random project.
  - Kill all your node processes.
  - Redo the launch.
- In the [Extension Development Host] instance of VSCode that just opened, open a `.res` file.
- Try various features.
- When you make a change, Go to the same Debug viewlet's Call Stack panel and restart the client and the server:

  <img width="359" alt="image" src="https://user-images.githubusercontent.com/1909539/97448639-19db0800-18ee-11eb-875a-d17cd1b141d1.png">

### Change the Grammar

The _real_ source of truth for our grammar is at https://github.com/rescript-lang/rescript-sublime. We port that `sublime-syntax` grammar over to this weaker TextMate language grammar for VSCode and the rest. There are some subtle differences between the 2 grammars; currently we manually sync between them.

- Modify `grammars/rescript.tmLanguage.json`.

For more grammar inspirations, check:
- [TypeScript's grammar](https://github.com/microsoft/TypeScript-TmLanguage/blob/a771bc4e79deeae81a01d988a273e300290d0072/TypeScript.YAML-tmLanguage)
- [Writing a TextMate Grammar: Some Lessons Learned](https://www.apeth.com/nonblog/stories/textmatebundle.html)

### Snippets

Snippets are also synced from https://github.com/rescript-lang/rescript-sublime. VSCode snippets docs [here](https://code.visualstudio.com/api/references/contribution-points#contributes.snippets).

### Autocomplete, Jump To Definition, Type Hint, Etc.

These are taken care of by the binary at [rescript-editor-support](https://github.com/rescript-lang/rescript-editor-support). We just invoke it in `RescriptEditorSupport.ts`.

## Binary Invocation

We call a few binaries and it's tricky to call them properly cross-platform. Here are some tips:

- We try to call the binaries synchronously to avoid races.
- Make sure you cater to calling a binary and passing e.g. a path with whitespace in it.
- `execFile` and its sync version do the above for free.
- `execFile` does not work on windows for batch scripts, which is what Node scripts are wrapped in. Use `exec`. See more [here](https://github.com/rescript-lang/rescript-vscode/blob/8fcc1ab428b8225c97d2c9a5b8e3a782c70d9439/server/src/utils.ts#L110).
- Thankfully, many of our binaries are native, so we can keep using `execFile` most of the time.

## Rough Description Of How The Plugin Works

### Editor Diagnostics

They should be synced in from `lib/bs/.compiler.log` build. Don't take them from other places.

### `.compiler.log`

The build output is streamed into `lib/bs/.compiler.log`. Here are its various states, numbered here:

1. Doesn't exist: artifacts not built yet, or cleaned away.
2. Present, without a final line `#Done`: still building.
3. Present, with the final line `#Done`: finished building.

Barring FS errors, there should be no other state to `.compiler.log`. Among others, this means the file is never present but empty.

The compiler log contains exactly the same things you'd see in a regular terminal `bsb` guild, except:

- The errors are indented 2 spaces
- The extra `#Start` and `#Done` (which aren't indented).

A parser for the diagnostics is [here](https://github.com/rescript-lang/rescript-vscode/blob/0dbf2eb9cdb0bd6d95be1aee88b73830feecb5cc/server/src/utils.ts#L129-L329).

### State 1

Artifacts cleaning through `bsb -clean` removes `.compiler.log` and turns into state 1. If that's the case, remove the diagnostics in the editor too. One could argue that they should be kept, but that's misleading UX-wise, and harder to implement correctly.

### Streaming Update of Diagnostics

After saving a file and running the build, the results stream into the log file. Unfortunately, UX-wise, in the editor, this might look like the diagnostics are suddenly gone then coming back in file by file. This looks bad. To remediate:

- If it's in state 2, update those particular files' diagnostics but don't wipe the files' diagnostics yet.
- If in state 3, finish by clean up the rest of the old diagnostics. This means there's a bit of bookeeping needed here. Make sure you get it right. It's possible for a build to be interrupted (and therefore state 4 never reached) and restarted.

Even this fix isn't great. Ideally, the editor's diagnostics can be greyed out while we're updating them...

Keep in mind that you might be tracking multiple `.compiler.log`s. You should do the above for each.

### Stale Diagnostics Detection

To check whether the artifacts are stale, do **not** check `.bsb.lock` at the project root. This is unreliable, since it's possible that `bsb` wasn't running in watcher mode. We also don't want to encourage overuse of the watcher mode, though it seems increasingly common.

We currently do that; we wish we aren't.

<!-- Instead, do `bsb.exe -- -n` -->

### Files from Other Projects

It's possible to open files from different projects into the same editor instance. In that case, also read _that_ file's project's `.compiler.log`.

The bad alternatives are:
- Not show that file's project's errors. That's wrong for several reasons (looks like the file has no error, assumes an editor window has a default project, etc.).
- Show only that file's error. That's just weird, the errors are already read from that project's `.compiler.log`. Might as well show all of them (?).

## Running `bsb` in the Editor

**Don't** do that unless you've prompted the user. This plugin currently prompts the user upon opening thr first the first file of a project. It's not great, but otherwise lots of folks forget to start a `bsb` in the terminal to see the freshest diagnostics.

Drawbacks:

- Running an implicit `bsb -w` means you've acquired the build watch mode lockfile. The user won't be able to run his/her own `bsb -w` in the terminal.
- Running a one-shot `bsb` doesn't conflict, but is a waste. It's also incorrect, as there might be external file system changes you're not detecting, e.g. version control changes.
- The build might be a step in a bigger build. The editor running `bsb -w` by itself might clash with that.
- If you have multiple files with different project roots open, running all of the `bsb -w`s is too intense.

## Format

To find the location of `bsc.exe` to run the formatter:
- Search in the file's directory's `node_modules/bs-platform/{platform}/bsc.exe`. If not found, recursively search upward (because [monorepos](https://github.com/rescript-lang/rescript-vscode/blob/0dbf2eb9cdb0bd6d95be1aee88b73830feecb5cc/server/src/utils.ts#L39-L45)).
	-	Do **not** directly use `node_modules/.bin/bsc` if you can help it. That's a Nodejs wrapper. Slow startup. We don't want our formatting to be momentarily stalled because some Nodejs cache went cold.
	- `platform` can be `darwin`, `linux`, `win32` or `freebsd`.

### Formatting Newline

The formatted result should be taken as-is, without any extra string trimming and newline addition/removal by the editor plugin.

### Formatting Errors

The errors returned from `bsc.exe -format` should be discarded; in theory, they should have been duplicates of the errors from `.compiler.log`.

In the future, we should consier showing the format errors when `.compiler.log` isn't found.

## Release

Currently the release is vetted and done by @chenglou.

- Bump the version properly in `package.json` and lockfile and make a new commit.
- Make sure @ryyppy is aware of your changes. He needs to sync them over to the vim plugin.
- Use `vsce publish` to publish. Official VSCode guide [here](https://code.visualstudio.com/api/working-with-extensions/publishing-extension). Only @chenglou has the publishing rights right now.
- Not done! Make a new manual release [here](https://github.com/rescript-lang/rescript-vscode/releases); use `vsce package` to package up a standalone `.vsix` plugin and attach it onto that new release. This is for folks who don't use the VSCode marketplace.
