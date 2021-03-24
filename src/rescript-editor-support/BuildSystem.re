let namespacedName = (namespace, name) =>
  switch (namespace) {
  | None => name
  | Some(namespace) => name ++ "-" ++ namespace
  };

open Infix;

let getBsPlatformDir = rootPath => {
  let result =
    ModuleResolution.resolveNodeModulePath(
      ~startPath=rootPath,
      "bs-platform",
    );
  let result =
    if (result == None) {
      ModuleResolution.resolveNodeModulePath(
        ~startPath=rootPath,
        "rescript",
      );
    } else {
      result;
    };
  switch (result) {
  | Some(path) => Ok(path)
  | None =>
    let message = "bs-platform could not be found";
    Log.log(message);
    Error(message);
  };
};

let getCompiledBase = root => {
  Files.ifExists(root /+ "lib" /+ "bs");
};
let getStdlib = base => {
  let%try_wrap bsPlatformDir = getBsPlatformDir(base);
  bsPlatformDir /+ "lib" /+ "ocaml";
};
