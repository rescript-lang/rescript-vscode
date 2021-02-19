open Infix;
open RResult;
open TopTypes;
module J = JsonShort;

let getTextDocument = doc => {
  let%opt uri = Json.get("uri", doc) |?> Json.string |?> Uri2.parse;
  let%opt text = Json.get("text", doc) |?> Json.string;
  Some((uri, text));
};

let notificationHandlers:
  list((string, (state, Json.t) => result(state, string))) = [
  (
    "textDocument/didOpen",
    (state, params) => {
      let%try (uri, text) =
        Json.get("textDocument", params)
        |?> getTextDocument
        |> RResult.orError("Invalid params");
      Hashtbl.replace(state.documentText, uri, text);

      let path = Uri2.toPath(uri);
      if (FindFiles.isSourceFile(path)) {
        let%try package = Packages.getPackage(uri, state);
        /* let name = FindFiles.getName(path); */
        if (!Hashtbl.mem(package.nameForPath, path)) {
          /* TODO: figure out what the name should be, and process it. */
          package.nameForPath
          |> Hashtbl.iter((name, _) => Log.log(" > " ++ name));
          Log.log("Reloading because you created a new file: " ++ path);
          Ok(state);
          /* Ok(reloadAllState(state)) */
          /* Hashtbl.add(package.nameForPath, path, name);
             Hashtbl.add(package.pathsForModule, name, Impl(path, Some(path)));
             Hashtbl.replace(state.packagesByRoot, package.basePath, {
               ...package,
               localModules: [name, ...package.localModules]
             });
             Ok(state) */
        } else {
          Ok(state);
        };
      } else {
        Ok(state);
      };
    },
  ),
  (
    "workspace/didChangeConfiguration",
    (state, _params) => {
      Ok(state);
    },
  ),
  (
    "textDocument/didSave",
    (state, _params) => {
      Ok(state);
    },
  ),
  (
    "textDocument/didChange",
    (state, params) => {
      open InfixResult;
      let%try doc = params |> RJson.get("textDocument");
      let%try uri = RJson.get("uri", doc) |?> RJson.string;
      let%try uri = Uri2.parse(uri) |> RResult.orError("Not a uri");
      let%try changes = RJson.get("contentChanges", params) |?> RJson.array;
      let%try text =
        List.nth(changes, List.length(changes) - 1)
        |> RJson.get("text")
        |?> RJson.string;
      /* Hmm how do I know if it's modified? */
      let state = State.updateContents(uri, text, state);
      Ok(state);
    },
  ),
  (
    "workspace/didChangeWatchedFiles",
    (state, _params) => {
      Ok(state);
    },
  ),
];
