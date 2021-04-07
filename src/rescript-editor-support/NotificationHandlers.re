open Infix;
open RResult;
open TopTypes;
module J = JsonShort;

let getTextDocument = doc =>
  switch (Json.get("uri", doc) |?> Json.string |?> Uri2.parse) {
  | None => None
  | Some(uri) =>
    switch (Json.get("text", doc) |?> Json.string) {
    | None => None
    | Some(text) => Some((uri, text))
    }
  };

let notificationHandlers:
  list((string, (state, Json.t) => result(state, string))) = [
  (
    "textDocument/didOpen",
    (state, params) => {
      switch (
        Json.get("textDocument", params)
        |?> getTextDocument
        |> RResult.orError("Invalid params")
      ) {
      | Error(e) => Error(e)
      | Ok((uri, text)) =>
        Hashtbl.replace(state.documentText, uri, text);
        let path = Uri2.toPath(uri);
        if (FindFiles.isSourceFile(path)) {
          switch (Packages.getPackage(uri, state)) {
          | Error(e) => Error(e)
          | Ok(package) =>
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
            }
          };
        } else {
          Ok(state);
        };
      }
    }
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
      switch (params |> RJson.get("textDocument")) {
      | Error(e) => Error(e)
      | Ok(doc) =>
        switch (RJson.get("uri", doc) |?> RJson.string) {
        | Error(e) => Error(e)
        | Ok(uri) =>
          switch (Uri2.parse(uri) |> RResult.orError("Not a uri")) {
          | Error(e) => Error(e)
          | Ok(uri) =>
            switch (RJson.get("contentChanges", params) |?> RJson.array) {
            | Error(e) => Error(e)
            | Ok(changes) =>
              switch (
                List.nth(changes, List.length(changes) - 1)
                |> RJson.get("text")
                |?> RJson.string
              ) {
              | Error(e) => Error(e)
              | Ok(text) =>
                /* Hmm how do I know if it's modified? */
                let state = State.updateContents(uri, text, state);
                Ok(state);
              }
            }
          }
        }
      }
    }
  ),
  (
    "workspace/didChangeWatchedFiles",
    (state, _params) => {
      Ok(state);
    },
  ),
];
