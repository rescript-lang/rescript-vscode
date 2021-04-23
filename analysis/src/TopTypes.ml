(* Aliases to make the intents clearer *)
type uri = Uri2.t

type filePath = string

type moduleName = string

(* Here are the things that will be different between jbuilder things *)
type package = {
  rootPath : filePath;
  (* Depend on bsb having already run *)
  localModules : moduleName list;
  interModuleDependencies : (moduleName, moduleName list) Hashtbl.t;
  dependencyModules : moduleName list;
  pathsForModule : (moduleName, SharedTypes.paths) Hashtbl.t;
  namespace : string option;
  opens : string list;
}

type state = {
  packagesByRoot : (string, package) Hashtbl.t;
  rootForUri : (uri, string) Hashtbl.t;
  cmtCache : (filePath, float * SharedTypes.file) Hashtbl.t;
}

let empty () =
  {
    packagesByRoot = Hashtbl.create 1;
    rootForUri = Hashtbl.create 30;
    cmtCache = Hashtbl.create 30;
  }
