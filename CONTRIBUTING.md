# Contributing

Thanks for your interest. Below is an informal spec of how the plugin's server communicates with the actual compiler. If you're a ReScript editor plugin implementor, you should probably read this to understand the various important nuances.

## Editor Diagnostics

They should be synced in from `lib/bs/.compiler.log` build. Don't take them from other places.

### `.compiler.log`

The build output is streamed into `lib/bs/.compiler.log`. Here are its various states, numbered here:

1. Doesn't exist: artifacts not built yet, or cleaned away.
2. Present, without a final line `#Done`: still building.
3. Present, with the final line `#Done`: finished building.

Barring FS errors, there should be no other state to `.compiler.log`. Among others, this means the file is never present but empty.

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

<!-- Instead, do `bsb.exe -- -n` -->

### Files from Other Projects

It's possible to open files from different projects into the same editor instance. In that case, also read _that_ file's project's `.compiler.log`.

The bad alternatives are:
- Not show that file's project's errors. That's wrong for several reasons (looks like the file has no error, assumes an editor window has a default project, etc.).
- Show only that file's error. That's just weird, the errors are already read from that project's `.compiler.log`. Might as well show all of them (?).

## Running `bsb` in the Editor

**Don't** do that unless you've prompted the user.

- Running an implicit `bsb -w` automatically means you've acquired the build watch mode lockfile. The user won't be able to run his/her own `bsb -w` in the terminal.
- Running a one-shot `bsb` doesn't conflict, but is a waste. It's also incorrect, as there might be external file system changes you're not detecting.
- The build might be a step in a bigger build. The editor running `bsb` implicitly goes against that.
- If you have multiple files with different project roots open, running all of the `bsb`s is too intense.

## Format

To find the location of `bsc` to run:
- Look for the nearest `bsconfig.json`. That's the root of the project. \*
- Search in that directory's `node_modules/bs-platform/{platform}/bsc.exe`.
	-	Do **not** directly use `node_modules/.bin/bsc` if you can help it. That's a Nodejs wrapper. Slow startup. We don't want our formatting to be momentarily stalled because some Nodejs cache went cold.
	- `platform` can be `darwin`, `linux`, `win32` or `freebsd`.

\*  Maybe we'll do this differently in the future because asking for a `bsconfig.json` just to format some files is rough.

### Formatting Newline

The formatted result should be taken as-is, without any extra string trimming and newline addition/removal by the editor plugin.

### Formatting Errors

The errors returned from `bsc.exe -format` should be discarded; in theory, they should have been duplicates of the errors from `.compiler.log`.

In the future, we should consier showing the format errors when `.compiler.log` isn't found.
