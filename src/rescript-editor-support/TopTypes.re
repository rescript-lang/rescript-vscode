/* Aliases to make the intents clearer */
type uri = string;
type filePath = string;
type moduleName = string;

/* Here are the things that will be different between jbuilder things */
type package = {
  rootPath: filePath,
  /* Might change based on bsconfig.json / .merlin */
  includeDirectories: list(filePath),
  compilationFlags: string,
  /* Depend on bsb having already run */
  localModules: list(moduleName),
  interModuleDependencies: Hashtbl.t(moduleName, list(moduleName)),
  dependencyModules: list(moduleName),
  pathsForModule: Hashtbl.t(moduleName, SharedTypes.paths),
  nameForPath: Hashtbl.t(filePath, moduleName),
  namespace: option(string),
  opens: list(string),
  tmpPath: string,
  compilerPath: filePath,
  refmtPath: option(filePath),
  /** TODO maybe make this general, so that I can support arbitrary syntaxes? */
  lispRefmtPath: option(filePath),
};

type settings = {
  perValueCodelens: bool,
  refmtLocation: option(string),
  lispRefmtLocation: option(string),
  opensCodelens: bool,
  dependenciesCodelens: bool,
  clientNeedsPlainText: bool,
  showModulePathOnHover: bool,
  recordAllLocations: bool,
};

type state = {
  rootUri: uri,
  settings,
  documentText: Hashtbl.t(uri, (string, int, bool)),
  packagesByRoot: Hashtbl.t(string, package),
  rootForUri: Hashtbl.t(uri, string),
  cmtCache: Hashtbl.t(filePath, (float, SharedTypes.file)),
  lastDefinitions: Hashtbl.t(uri, SharedTypes.full),
};

let empty = () => {
  rootUri: "- uninitialized -",
  documentText: Hashtbl.create(5),
  packagesByRoot: Hashtbl.create(1),
  rootForUri: Hashtbl.create(30),
  cmtCache: Hashtbl.create(30),
  lastDefinitions: Hashtbl.create(10),
  settings: {
    refmtLocation: None,
    lispRefmtLocation: None,
    perValueCodelens: false,
    opensCodelens: true,
    dependenciesCodelens: true,
    clientNeedsPlainText: false,
    showModulePathOnHover: true,
    recordAllLocations: false,
  },
};
