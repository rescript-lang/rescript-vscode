open RResult;
open TopTypes;
open Infix;
module J = JsonShort;

let handlers:
  list((string, (state, Json.t) => result((state, Json.t), string))) = [
  (
    "textDocument/definition",
    (state, params) => {
      switch (Protocol.rPositionParams(params)) {
      | Error(e) => Error(e)
      | Ok((uri, pos)) =>
        switch (State.getFullFromCmt(~state, ~uri)) {
        | Error(e) => Error(e)
        | Ok((package, {file, extra})) =>
          let position = Utils.cmtLocFromVscode(pos);
          (
            switch (
              References.definitionForPos(
                ~pathsForModule=package.pathsForModule,
                ~file,
                ~extra,
                ~getUri=State.fileForUri(state),
                ~getModule=State.fileForModule(state, ~package),
                position,
              )
            ) {
            | None => None
            | Some((uri, loc)) =>
              Some(
                Ok((
                  state,
                  Json.Object([
                    ("uri", Json.String(Uri2.toString(uri))),
                    ("range", Protocol.rangeOfLoc(loc)),
                  ]),
                )),
              );
            }
          )
          |? Ok((state, Json.Null));
        }
      }
    }
  ),
  (
    "textDocument/completion",
    (state, params) => {
      switch (Protocol.rPositionParams(params)) {
      | Error(e) => Error(e)
      | Ok((uri, pos)) =>
        let maybeText =
          switch (Hashtbl.find_opt(state.documentText, uri)) {
          | Some(text) => Some(text)
          | None => None
          };
        switch (State.getFullFromCmt(~state, ~uri)) {
        | Error(e) => Error(e)
        | Ok((package, full)) =>
          let completions =
            NewCompletions.computeCompletions(
              ~full,
              ~maybeText,
              ~package,
              ~pos,
              ~state,
            );
          Ok((state, completions));
        };
      }
    }
  ),
  (
    "textDocument/documentHighlight",
    (state, params) => {
      switch (Protocol.rPositionParams(params)) {
      | Error(e) => Error(e)
      | Ok((uri, pos)) =>
        let pos = Utils.cmtLocFromVscode(pos);
        let refs =
          switch (State.fileForUri(state, uri) |> toOptionAndLog) {
          | None => None
          | Some((file, extra)) => References.refsForPos(~file, ~extra, pos)
          };
        Ok((
          state,
          switch (refs) {
          | None => J.null
          | Some(refs) =>
            J.l(
              refs
              |> List.map(loc =>
                   J.o([
                     ("range", Protocol.rangeOfLoc(loc)),
                     ("kind", J.i(2)),
                   ])
                 ),
            )
          },
        ));
      }
    }
  ),
  (
    "textDocument/references",
    (state, params) => {
      switch (Protocol.rPositionParams(params)) {
      | Error(e) => Error(e)
      | Ok((uri, pos)) =>
        switch (Packages.getPackage(uri, state)) {
        | Error(e) => Error(e)
        | Ok(package) =>
          switch (State.fileForUri(state, uri)) {
          | Error(e) => Error(e)
          | Ok((file, extra)) =>
            Ok(
              Infix.(
                (
                  switch (
                    References.locForPos(~extra, Utils.cmtLocFromVscode(pos))
                  ) {
                  | None => None
                  | Some((_, loc)) =>
                    switch (
                      References.allReferencesForLoc(
                        ~pathsForModule=package.pathsForModule,
                        ~file,
                        ~extra,
                        ~allModules=package.localModules,
                        ~getUri=State.fileForUri(state),
                        ~getModule=State.fileForModule(state, ~package),
                        ~getExtra=State.extraForModule(state, ~package),
                        loc,
                      )
                      |> toOptionAndLog
                    ) {
                    | None => None
                    | Some(allReferences) =>
                      Some((
                        state,
                        J.l(
                          allReferences
                          |> List.map(((fname, references)) => {
                               let locs =
                                 fname == uri
                                   ? List.filter(
                                       loc => !Protocol.locationContains(loc, pos),
                                       references,
                                     )
                                   : references;
                               locs
                               |> List.map(loc => Protocol.locationOfLoc(~fname, loc));
                             })
                          |> List.concat,
                        ),
                      ));
                    }
                  }
                )
                |? (state, J.null)
              ),
            )
          }
        }
      }
    }
  ),
  (
    "textDocument/rename",
    (state, params) => {
      switch (Protocol.rPositionParams(params)) {
      | Error(e) => Error(e)
      | Ok((uri, pos)) =>
        switch (Packages.getPackage(uri, state)) {
        | Error(e) => Error(e)
        | Ok(package) =>
          switch (State.fileForUri(state, uri)) {
          | Error(e) => Error(e)
          | Ok((file, extra)) =>
            switch (RJson.get("newName", params)) {
            | Error(e) => Error(e)
            | Ok(newName) =>
              open Infix;
              (
                switch (
                  References.locForPos(~extra, Utils.cmtLocFromVscode(pos))
                ) {
                | None => None
                | Some((_, loc)) =>
                  switch (
                    References.allReferencesForLoc(
                      ~file,
                      ~extra,
                      ~pathsForModule=package.pathsForModule,
                      ~allModules=package.localModules,
                      ~getModule=State.fileForModule(state, ~package),
                      ~getUri=State.fileForUri(state),
                      ~getExtra=State.extraForModule(state, ~package),
                      loc,
                    )
                    |> toOptionAndLog
                  ) {
                  | None => None
                  | Some(allReferences) =>
                    Some(
                      Ok((
                        state,
                        J.o([
                          (
                            "changes",
                            J.o(
                              allReferences
                              |> List.map(((fname, references)) =>
                                    (
                                      fname |> Uri2.toString,
                                      J.l(
                                        references
                                        |> List.map(loc =>
                                            J.o([
                                              ( "range", Protocol.rangeOfLoc(loc)),
                                              ("newText", newName),
                                            ])
                                          ),
                                      ),
                                    )
                                  ),
                            ),
                          ),
                        ]),
                      ),
                    ))
                  }
                }
              )
              |? Ok((state, J.null))
            }
          }
        }
      }
    }
  ),
  (
    "textDocument/codeLens",
    (state, params) => {
      open InfixResult;
      switch (
        params
        |> RJson.get("textDocument")
        |?> RJson.get("uri")
        |?> RJson.string
      ) {
      | Error(e) => Error(e)
      | Ok(uri) =>
        switch (Uri2.parse(uri) |> RResult.orError("Not a uri")) {
        | Error(e) => Error(e)
        | Ok(uri) =>
          /* Log.log("<< codleens me please"); */
          let topLoc = {
            Location.loc_start: {
              Lexing.pos_fname: "",
              pos_lnum: 1,
              pos_bol: 0,
              pos_cnum: 0,
            },
            Location.loc_end: {
              Lexing.pos_fname: "",
              pos_lnum: 1,
              pos_bol: 0,
              pos_cnum: 0,
            },
            loc_ghost: false,
          };
          let getLensItems = ({SharedTypes.file, extra}) => {
            /* getTypeLensTopLevel gives the list of per-value type codeLens
              for every value in a module topLevel */
            let rec getTypeLensTopLevel = topLevel => {
              switch (topLevel) {
              | [] => []
              | [{SharedTypes.name: {loc}, item}, ...tlp] =>
                let currentCl =
                  switch (item) {
                  | SharedTypes.MValue(typ) => [
                      (typ |> Shared.typeToString, loc),
                    ]
                  | Module(Structure({topLevel})) =>
                    getTypeLensTopLevel(topLevel)
                  | _ => []
                  };
                List.append(currentCl, getTypeLensTopLevel(tlp));
              };
            };
            let lenses = file.contents.topLevel |> getTypeLensTopLevel;
            let lenses = lenses @ CodeLens.forOpens(extra);
            let depsList =
              List.map(fst, SharedTypes.hashList(extra.externalReferences));
            let depsString =
              depsList == [] ? "[none]" : String.concat(", ", depsList);
            let lenses = [("Dependencies: " ++ depsString, topLoc), ...lenses];
            lenses;
          };
          let items =
            switch (State.getFullFromCmt(~state, ~uri)) {
            | Error(message) => [(message, topLoc)]
            | Ok((_package, full)) => getLensItems(full)
            };
          Ok((
            state,
            J.l(
              items
              |> List.map(((text, loc)) =>
                    J.o([
                      ("range", Protocol.rangeOfLoc(loc)),
                      (
                        "command",
                        J.o([("title", J.s(text)), ("command", J.s(""))]),
                      ),
                    ])
                  ),
            ),
          ));
        }
      }

    }
  ),
  (
    "textDocument/hover",
    (state, params) => {
      switch (Protocol.rPositionParams(params)) {
      | Error(e) => Error(e)
      | Ok((uri, pos)) =>
        switch (Packages.getPackage(uri, state)) {
        | Error(e) => Error(e)
        | Ok(package) =>
          switch (State.fileForUri(state, uri)) {
          | Error(e) => Error(e)
          | Ok((file, extra)) =>
            {
              let pos = Utils.cmtLocFromVscode(pos);
              switch (References.locForPos(~extra, pos)) {
              | None => None
              | Some((location, loc)) =>
                switch (
                  Hover.newHover(
                    ~file,
                    ~getModule=State.fileForModule(state, ~package),
                    loc,
                  )
                ) {
                | None => None
                | Some(text) =>
                  Some(
                    Ok((
                      state,
                      J.o([
                        ("range", Protocol.rangeOfLoc(location)),
                        ("contents", text |> Protocol.contentKind),
                      ]),
                    ),
                  ))
                }
              };
            }
            |? Ok((state, J.null))
          }
        }
      }
    }
  ),
  (
    "textDocument/documentSymbol",
    (state, params) => {
      open InfixResult;
      switch (
        params
        |> RJson.get("textDocument")
        |?> RJson.get("uri")
        |?> RJson.string
      ) {
      | Error(e) => Error(e)
      | Ok(uri) =>
        switch (Uri2.parse(uri) |> RResult.orError("Not a uri")) {
        | Error(e) => Error(e)
        | Ok(uri) =>
          switch (State.fileForUri(state, uri)) {
          | Error(e) => Error(e)
          | Ok((file, _extra)) =>
            open SharedTypes;

            let rec getItems = ({topLevel}) => {
              let fn = ({name: {txt}, extentLoc, item}) => {
                let (item, siblings) =
                  switch (item) {
                  | MValue(v) => (v |> Shared.variableKind, [])
                  | MType(t, _) => (t.decl |> Shared.declarationKind, [])
                  | Module(Structure(contents)) => (Module, getItems(contents))
                  | Module(Ident(_)) => (Module, [])
                  };
                if (extentLoc.loc_ghost) {
                  siblings;
                } else {
                  [(txt, extentLoc, item), ...siblings];
                };
              };
              let x = topLevel |> List.map(fn) |> List.concat;
              x;
            };

            getItems(file.contents)
            |> (
              items => {
                Ok((
                  state,
                  J.l(
                    items
                    |> List.map(((name, loc, typ)) =>
                          J.o([
                            ("name", J.s(name)),
                            ("kind", J.i(Protocol.symbolKind(typ))),
                            ("location", Protocol.locationOfLoc(loc)),
                            /* ("containerName", s(String.concat(".", path))) */
                          ])
                        ),
                  ),
                ));
              }
            );
          }
        }
      }
    }
  ),
];
