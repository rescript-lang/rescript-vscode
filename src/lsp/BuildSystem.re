
type compilerVersion =
  | V406;

let showCompilerVersion = fun
  | V406 => "4.06";

type t =
  | Bsb(string);

let fromString = string => {
  switch (Utils.split_on_char(':', string)) {
    | ["bsb", version] => Some(Bsb(version))
    | _ => None
  }
};

let show = t => switch t {
  | Bsb(v) => "bsb version " ++ v
}

let getLine = (cmd, ~pwd) => {
  switch (Commands.execFull(~pwd, cmd)) {
    | ([line], _, true) => RResult.Ok(line)
    | (out, err, _) => Error("Invalid response for " ++ cmd ++ "\n\n" ++ String.concat("\n", out @ err))
  }
};

let namespacedName = (namespace, name) => switch namespace {
  | None => name
  | Some(namespace) => name ++ "-" ++ namespace
};

open Infix;

let nodePlatform = 
    switch (Sys.os_type) {
      | "Unix" => switch (input_line (Unix.open_process_in ("uname -s"))) {
         | "Darwin"  => "darwin"
         | "Linux"   => "linux"
         | "FreeBSD" => "freebsd"
         | s => invalid_arg (s ++ ": unsupported os_type")
      }
      | "Win32" => "win32"
      | s => invalid_arg (s ++ ": unsupported os_type")
  };


let getBsPlatformDir = rootPath => {
  let result =
    ModuleResolution.resolveNodeModulePath(
      ~startPath=rootPath,
      "bs-platform",
    );
  switch (result) {
  | Some(path) =>
    RResult.Ok(path);
  | None =>
    let resultSecondary =
      ModuleResolution.resolveNodeModulePath(
        ~startPath=rootPath,
        "bsb-native",
      );
    switch (resultSecondary) {
    | Some(path) => RResult.Ok(path)
    | None =>
      let message = "bs-platform could not be found";
      Log.log(message);
      RResult.Error(message);
    }
  };
};

/* One dir up, then into .bin.
    Is .bin always in the modules directory?
   */
let getBsbExecutable = rootPath =>
  RResult.InfixResult.(
    getBsPlatformDir(rootPath)
    |?>> Filename.dirname
    |?>> (path => path /+ ".bin" /+ "bsb")
  );

let parseOCamlVersion = versionString =>switch (Utils.split_on_char('.', String.trim(versionString))) {
    | ["4", "06", ..._] => Ok(V406)
    | _ => Error("Unsupported OCaml version: " ++ versionString)
  }

let getCompilerVersion = executable => {
  let cmd = executable ++ " -version";
  let (output, success) = Commands.execSync(cmd);
  success ? switch output {
  | [line] when Str.string_match(Str.regexp_string("BuckleScript "), line, 0) =>
    switch (Str.split(Str.regexp("( *Using OCaml:?"), String.trim(line))) {
      | [_, version] => parseOCamlVersion(version)
      | xs => Error("Cannot detect OCaml version from BuckleScript version string: " ++ line ++ "[" ++ String.concat(" ;", xs) ++ "]")
    }
  | [line] => parseOCamlVersion(line)
  | _ => Error("Unable to determine compiler version (ran " ++ cmd ++ "). Output: " ++ String.concat("\n", output))
  } : Error("Could not run compiler (ran " ++ cmd ++ "). Output: " ++ String.concat("\n", output));
};

let detect = (rootPath, bsconfig) => {
  let%try bsbExecutable = getBsbExecutable(rootPath);
  let%try_wrap bsbVersion = {
    let cmd = bsbExecutable ++ " -version";
    let (output, success) = Commands.execSync(cmd);
    success ? switch output {
    | [line] => Ok(String.trim(line))
    | _ => Error("Unable to determine bsb version (ran " ++ cmd ++ "). Output: " ++ String.concat("\n", output))
    } : Error("Could not run bsb (ran " ++ cmd ++ "). Output: " ++ String.concat("\n", output));
  };

  Bsb(bsbVersion);
};

let detectFull = projectDir => {
  let bsConfig = Filename.concat(projectDir, "bsconfig.json");
  if (Files.exists(bsConfig)) {
    let%try raw = Files.readFileResult(bsConfig);
    let config = Json.parse(raw);
    detect(projectDir, config);
  } else {
    Error("Couldn't find bsconfig.json")
  };
};


let getCompiledBase = (root, buildSystem) => {
  let compiledBase = switch (buildSystem) {
  | Bsb("3.2.0") => Ok(root /+ "lib" /+ "bs" /+ "js")
  | Bsb("3.1.1") => Ok(root /+ "lib" /+ "ocaml")
  | Bsb(_) => Ok(root /+ "lib" /+ "bs")
  };

  switch compiledBase {
  | Ok(compiledBase) => Files.ifExists(compiledBase);
  | _ => None
  };
};

let getStdlib = (base) => {
  let%try_wrap bsPlatformDir = getBsPlatformDir(base);
  [bsPlatformDir /+ "lib" /+ "ocaml"]
};


let getCompiler = (rootPath) => {
  let%try_wrap bsPlatformDir = getBsPlatformDir(rootPath);
  switch(Files.ifExists(bsPlatformDir /+ "lib" /+ "bsc.exe")){
    | Some (x) => x 
    | None => bsPlatformDir /+ nodePlatform /+ "bsc.exe"
  }
};

let getRefmt = (rootPath, buildSystem) => {
  let bsRefmt = (bsPlatformDir) =>
    switch (Files.ifExists(bsPlatformDir/+"lib"/+"refmt.exe")){
      | Some (x) => x
      | None => 
        switch(Files.ifExists(bsPlatformDir /+ nodePlatform /+ "refmt.exe")){
          | Some (x) => x 
          | None => bsPlatformDir /+ "lib" /+ "refmt3.exe"
        }
    }  
  switch (buildSystem) {
    | Bsb(version) when version > "2.2.0" =>
      let%try_wrap bsPlatformDir = getBsPlatformDir(rootPath);
      bsRefmt(bsPlatformDir)
    | Bsb(_) =>
      let%try_wrap bsPlatformDir = getBsPlatformDir(rootPath);
      bsRefmt(bsPlatformDir)
  };
};

let hiddenLocation = (rootPath) => {
  Ok(rootPath /+ "node_modules" /+ ".lsp")
};
