/* Aliases to make the intents clearer */
type uri = Uri2.t;
type filePath = string;
type moduleName = string;

/* Here are the things that will be different between jbuilder things */
type package = {
  rootPath: filePath,
  /* Depend on bsb having already run */
  localModules: list(moduleName),
  interModuleDependencies: Hashtbl.t(moduleName, list(moduleName)),
  dependencyModules: list(moduleName),
  pathsForModule: Hashtbl.t(moduleName, SharedTypes.paths),
  namespace: option(string),
  opens: list(string),
};

type state = {
  packagesByRoot: Hashtbl.t(string, package),
  rootForUri: Hashtbl.t(uri, string),
  cmtCache: Hashtbl.t(filePath, (float, SharedTypes.file)),
};

let empty = () => {
  packagesByRoot: Hashtbl.create(1),
  rootForUri: Hashtbl.create(30),
  cmtCache: Hashtbl.create(30),
};
