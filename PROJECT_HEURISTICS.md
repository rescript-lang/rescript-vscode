# Project heuristics

Vscode and vim need to be able to detect the correct location of the `.compiler.log` and `bsc.exe` for diagnostics and formatting.
Different project layouts result into different locations. The aim of this document is to formalize the different cases and specify where the editor tooling should search for `.compiler.log` and `bsc.exe` 

## Heuristics

1. `.compiler.log` (diagnostics)
- Starting from the current open file, walk the filesystem upwards, until `bsconfig.json` is located
- `.compiler.log` is located at `path.join(foundDirectory, "lib", "bs", ".compiler.log")`

2. platform specific `bsc.exe` (formatter)
- Starting from the current open file, walk the filesystem upwards, until `./node_modules/bs-platform` is located
- `bsc.exe` is located at `path.join(foundDirectory, "./node_modules/bs-platform", platform, "bsc.exe")`

3. `bsb` (start a build/get freshest data)
- Starting from the current open file, walk the filesystem upwards, until `./node_modules/.bin/bsb` is located
- `bsb` is located at `path.join(foundDirectory, "./node_modules/.bin", "bsb")`

## Cases
### Simple project

```
/folder1
  /src
    App.res
  /lib
    /bs
      .compiler.log
  /node_modules
    /bs-platform
      /darwin|linux|win32
        bsc.exe
    /.bin
      bsb
  package.json
  bsconfig.json
```

Typical workflow:
1. run `yarn` or `npm install` in `/folder1`
2. build: `npx/yarn bsb` in `/folder1`

When `/folder1/src/App.res` is openend in the editor:
1. `bsc.exe` can be found in `folder1/node_modules/bs-platform/{platform}/bsc.exe`
2. `.compiler.log` can be found in `folder1/lib/bs/.compiler.log`
3. `bsb` to get a build for this project and get the freshest data: `folder1/node_modules/.bin/bsb`


### Monorepo with yarn workspace with one ReScript project in subfolder
```
/root
  /folder1
    /src
      App.res
    /lib
      /bs
        .compiler.log
    /node_modules
      /.bin
        bsb
    package.json
    bsconfig.json
  /node_modules
    /bs-platform
      /darwin|linux|win32
        bsc.exe
    /.bin
      bsb
  package.json
  yarn.lock
```

Typical workflow:
1. run `yarn` in the root
2. build folder1: `cd folder1 && yarn bsb`

When `/folder1/src/App.res` is openend in the editor:
1. `bsc.exe` can be found in `/root/node_modules/bs-platform/{platform}/bsc.exe`
2. `.compiler.log` can be found in `/root/folder1/lib/bs/.compiler.log`
3. `bsb` to get a build for this project and get the freshest data: `/root/folder1/node_modules/.bin/bsb`. Note that there's also a one in `/root/node_modules/.bin/bsb`. The bsb in the subfolder is a symlink to the one in the root's node_modules. Bsb needs a `bsconfig.json` in the current working directory, so the cwd for bsb needs to be `/root/folder1`

### Yarn workspace with multiple ReScript projects in subfolders

Monorepo with two subfolders `folder1` and `folder2` on the same bs-platform version.

```
/root
  /folder1
    /src
      Folder1App.res
    /lib
      /bs
        .compiler.log
    /node_modules
      /.bin
        bsb
    package.json
    bsconfig.json
  /folder2
    /src
      Folder2App.res
    /lib
      /bs
        .compiler.log
    /node_modules
      /.bin
        bsb
    package.json
    bsconfig.json
  /node_modules
    /bs-platform
      /darwin|linux|win32
        bsc.exe
    /.bin
      bsb
  package.json
  yarn.lock
```

Typical workflow:
1. run `yarn` in the root
2. build folder1: `cd folder1 && yarn bsb`
3. build folder2: `cd folder2 && yarn bsb`


When `/root/folder1/src/Folder1App.res` is openend in the editor:
1. `bsc.exe` can be found in `/root/node_modules/bs-platform/{platform}/bsc.exe`
2. `.compiler.log` can be found in `/root/folder1/lib/bs/.compiler.log`
3. `bsb` to get a build for this project and get the freshest data: `/root/folder1/node_modules/.bin/bsb`. Note that there's also a one in `/root/node_modules/.bin/bsb`. The bsb in the subfolder is a symlink to the one in the root's node_modules. Bsb needs a `bsconfig.json` in the current working directory, so the cwd needs for bsb to be `/root/folder1`

When `/root/folder2/src/Folder2App.res` is openend in the editor:
1. `bsc.exe` can be found in `/root/node_modules/bs-platform/{platform}/bsc.exe`
2. `.compiler.log` can be found in `/root/folder2/lib/bs/.compiler.log`
3. `bsb` to get a build for this project and get the freshest data: `/root/folder2/node_modules/.bin/bsb`. Note that there's also a one in `/root/node_modules/.bin/bsb`. The bsb in the subfolder is a symlink to the one in the root's node_modules. Bsb needs a `bsconfig.json` in the current working directory, so the cwd for bsb needs to be `/root/folder2`

### Yarn workspace with multiple ReScript projects in subfolders where one is on a different bs-platform version

Monorepo where `folder1` and `folder2` are on `bs-platform` 8.4.2 and `folder3` on 8.3.3

```
  /folder1
    /src
      Folder1App.res
    /lib
      /bs
        .compiler.log
    /node_modules
      /.bin
        bsb --> symlink to /root/node_modules/.bin/bsb
    package.json
    bsconfig.json
  /folder2
    /src
      Folder2App.res
    /lib
      /bs
        .compiler.log
    /node_modules
      /.bin
        bsb --> symlink to /root/node_modules/.bin/bsb
    package.json
    bsconfig.json
  /folder3
    /src
      Folder3App.res
    /lib
      /bs
        .compiler.log
    /node_modules --> contains bs-platform with 8.3.3
      /.bin
        bsb --> 8.3.3
      /bs-platform
        /darwin|linux|win32
          bsc.exe --> 8.3.3
    package.json
    bsconfig.json
  /node_modules
    /bs-platform
      /darwin|linux|win32
        bsc.exe
    /.bin
      bsb
  package.json
  yarn.lock
```

Typical workflow:
1. run `yarn` in the root
2. build folder1: `cd folder1 && yarn bsb`
3. build folder2: `cd folder2 && yarn bsb`
4. build folder3: `cd folder3 && ./node_modules/.bin/bsb` (if you want to run bsb 8.3.3)

When `/root/folder1/src/Folder1App.res` is openend in the editor:
1. `bsc.exe` can be found in `/root/node_modules/bs-platform/{platform}/bsc.exe`
2. `.compiler.log` can be found in `/root/folder1/lib/bs/.compiler.log`
3. `bsb` to get a build for this project and get the freshest data: `/root/folder1/node_modules/.bin/bsb`. Note that there's also a one in `/root/node_modules/.bin/bsb`. The bsb in the subfolder is a symlink to the one in the root's node_modules. Bsb needs a `bsconfig.json` in the current working directory, so the cwd needs to be `/root/folder1`

When `/root/folder2/src/Folder2App.res` is openend in the editor:
1. `bsc.exe` can be found in `/root/node_modules/bs-platform/{platform}/bsc.exe`
2. `.compiler.log` can be found in `/root/folder2/lib/bs/.compiler.log`
3. `bsb` to get a build for this project and get the freshest data: `/root/folder2/node_modules/.bin/bsb`. Note that there's also a one in `/root/node_modules/.bin/bsb`. The bsb in the subfolder is a symlink to the one in the root's node_modules. Bsb needs a `bsconfig.json` in the current working directory, so the cwd for bsb needs to be `/root/folder2`

When `/root/folder3/src/Folder3App.res` is openend in the editor:
1. `bsc.exe` can be found in `/root/folder3/node_modules/bs-platform/{platform}/bsc.exe`
2. `.compiler.log` can be found in `/root/folder3/lib/bs/.compiler.log`
3. `bsb` to get a build for this project and get the freshest data: `/root/folder3/node_modules/.bin/bsb`. Here we need to use the `bsb` from `/root/folder3/node_modules/.bin/bsb` to get the correct 8.3.3 bsb!

### Yarn workspace with the "pinned dependencies" ReScript feature.

Monorepo with two folders: `folder1` and `folder2` on the same bs-platform version. The `bsconfig.json` of `folder1` contains: `"pinned-dependencies": ["folder2"]`.

```
/root
  /folder1
    /src
      Folder1App.res
    /lib
      /bs
        .compiler.log
    /node_modules
      /.bin
        bsb
    package.json
    bsconfig.json
  /folder2
    /src
      Folder2App.res
    /lib
      /bs
        .compiler.log
    /node_modules
      /.bin
        bsb
    package.json
    bsconfig.json
  /node_modules
    /bs-platform
      /darwin|linux|win32
        bsc.exe
    /.bin
      bsb
  package.json
  yarn.lock
```

Typical workflow:
- install node_modules: `yarn` from `/root`
- build: run `yarn bsb -make-world` from `/folder1` to build `/folder1` and `/folder2`

When `/root/folder1/src/Folder1App.res` is openend in the editor:
1. `bsc.exe` can be found in `/root/node_modules/bs-platform/{platform}/bsc.exe`
2. `.compiler.log` can be found in `/root/folder1/lib/bs/.compiler.log`
3. `bsb` to get a build for this project and get the freshest data: `/root/folder1/node_modules/.bin/bsb`. Note that there's also a one in `/root/node_modules/.bin/bsb`. The bsb in the subfolder is a symlink to the one in the root's node_modules. Bsb needs a `bsconfig.json` in the current working directory, so the cwd for bsb needs to be `/root/folder1`

When `/root/folder2/src/Folder2App.res` is openend in the editor:
1. `bsc.exe` can be found in `/root/node_modules/bs-platform/{platform}/bsc.exe`
2. `.compiler.log` can be found in `/root/folder2/lib/bs/.compiler.log`
3. `bsb` to get a build for this project and get the freshest data: `/root/folder2/node_modules/.bin/bsb`. Note that there's also a one in `/root/node_modules/.bin/bsb`. The bsb in the subfolder is a symlink to the one in the root's node_modules. Bsb needs a `bsconfig.json` in the current working directory, so the cwd for bsb needs to be `/root/folder2`
