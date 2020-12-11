open RResult;
open TopTypes;
open Infix;
module J = JsonShort;

let maybeHash = (h, k) =>
  if (Hashtbl.mem(h, k)) {
    Some(Hashtbl.find(h, k));
  } else {
    None;
  };

let getPackage =
  Packages.getPackage(
    ~reportDiagnostics=NotificationHandlers.reportDiagnostics,
  );

let handlers:
  list((string, (state, Json.t) => result((state, Json.t), string))) = [
  (
    "textDocument/definition",
    (state, params) => {
      let%try (uri, pos) = Protocol.rPositionParams(params);
      let%try package = getPackage(uri, state);
      let%try (file, extra) = State.fileForUri(state, ~package, uri);

      let position = Utils.cmtLocFromVscode(pos);

      {
        let%opt (uri, loc) =
          References.definitionForPos(
            ~pathsForModule=package.pathsForModule,
            ~file,
            ~extra,
            ~getUri=State.fileForUri(state, ~package),
            ~getModule=State.fileForModule(state, ~package),
            position,
          );
        Some(
          Ok((
            state,
            Json.Object([
              ("uri", Json.String(uri)),
              ("range", Protocol.rangeOfLoc(loc)),
            ]),
          )),
        );
      }
      |? Ok((state, Json.Null));
    },
  ),
  (
    "textDocument/completion",
    (state, params) => {
      let%try (uri, pos) = Protocol.rPositionParams(params);
      let maybeText =
        switch (Hashtbl.find_opt(state.documentText, uri)) {
        | Some((text, _version, _isClean)) => Some(text)
        | None => None
        };
      let%try package = getPackage(uri, state);
      let%try full = State.getBestDefinitions(uri, state, ~package);
      let completions =
        NewCompletions.computeCompletions(
          ~full,
          ~maybeText,
          ~package,
          ~pos,
          ~state,
        );
      Ok((state, completions));
    },
  ),
  (
    "textDocument/documentHighlight",
    (state, params) => {
      let%try (uri, pos) = Protocol.rPositionParams(params);
      let%try package = getPackage(uri, state);

      let res =
        {
          let pos = Utils.cmtLocFromVscode(pos);
          let%opt (file, extra) =
            State.fileForUri(state, ~package, uri) |> toOptionAndLog;

          let%opt_wrap refs = References.refsForPos(~file, ~extra, pos);
          (
            state,
            J.l(
              refs
              |> List.map(loc =>
                   J.o([
                     ("range", Protocol.rangeOfLoc(loc)),
                     ("kind", J.i(2)),
                   ])
                 ),
            ),
          );
        }
        |? (state, Json.Null);

      Ok(res);
    },
  ),
  (
    "textDocument/references",
    (state, params) => {
      let%try (uri, pos) = Protocol.rPositionParams(params);
      let%try package = getPackage(uri, state);
      let%try_wrap (file, extra) = State.fileForUri(state, ~package, uri);
      Infix.(
        {
          let%opt (_, loc) =
            References.locForPos(~extra, Utils.cmtLocFromVscode(pos));
          let%opt allReferences =
            References.allReferencesForLoc(
              ~pathsForModule=package.pathsForModule,
              ~file,
              ~extra,
              ~allModules=package.localModules,
              ~getUri=State.fileForUri(state, ~package),
              ~getModule=State.fileForModule(state, ~package),
              ~getExtra=State.extraForModule(state, ~package),
              loc,
            )
            |> toOptionAndLog;
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
        |? (state, J.null)
      );
    },
  ),
  (
    "textDocument/rename",
    (state, params) => {
      let%try (uri, pos) = Protocol.rPositionParams(params);
      let%try package = getPackage(uri, state);
      let%try (file, extra) = State.fileForUri(state, ~package, uri);
      let%try newName = RJson.get("newName", params);
      Infix.(
        {
          let%opt (_, loc) =
            References.locForPos(~extra, Utils.cmtLocFromVscode(pos));
          let%opt allReferences =
            References.allReferencesForLoc(
              ~file,
              ~extra,
              ~pathsForModule=package.pathsForModule,
              ~allModules=package.localModules,
              ~getModule=State.fileForModule(state, ~package),
              ~getUri=State.fileForUri(state, ~package),
              ~getExtra=State.extraForModule(state, ~package),
              loc,
            )
            |> toOptionAndLog;
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
                           fname,
                           J.l(
                             references
                             |> List.map(loc =>
                                  J.o([
                                    ("range", Protocol.rangeOfLoc(loc)),
                                    ("newText", newName),
                                  ])
                                ),
                           ),
                         )
                       ),
                  ),
                ),
              ]),
            )),
          );
        }
        |? Ok((state, J.null))
      );
    },
  ),
  (
    "textDocument/codeLens",
    (state, params) => {
      open InfixResult;
      let%try uri =
        params
        |> RJson.get("textDocument")
        |?> RJson.get("uri")
        |?> RJson.string;
      /* let%try package = getPackage(uri, state); */
      switch (getPackage(uri, state)) {
      | Error(message) =>
        let items = [
          (
            "Unable to load compilation data: " ++ message,
            {
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
            },
          ),
        ];
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
      | Ok(package) =>
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
          let showToplevelTypes = state.settings.perValueCodelens; /* TODO config option */
          let lenses =
            showToplevelTypes
              ? file.contents.topLevel |> getTypeLensTopLevel : [];
          let showOpens = state.settings.opensCodelens;
          let lenses =
            showOpens
              ? lenses
                @ {
                  CodeLens.forOpens(extra);
                }
              : lenses;

          let depsList =
            List.map(fst, SharedTypes.hashList(extra.externalReferences));
          let depsString =
            depsList == [] ? "[none]" : String.concat(", ", depsList);
          let lenses =
            state.settings.dependenciesCodelens == true
              ? [("Dependencies: " ++ depsString, topLoc), ...lenses]
              : lenses;

          lenses;
        };

        let items = {
          let full =
            switch (State.getCompilationResult(uri, state, ~package)) {
            | Ok(Success(_, full)) => Ok(Some(full))
            | Ok(_) => Ok(State.getLastDefinitions(uri, state))
            | Error(m) => Error(m)
            };
          switch (full) {
          | Error(message) => [(message, topLoc)]
          | Ok(None) => [("Unable to get compilation data", topLoc)]
          | Ok(Some(full)) => getLensItems(full)
          };
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
      };
    },
  ),
  (
    "textDocument/hover",
    (state, params) => {
      let%try (uri, pos) = Protocol.rPositionParams(params);
      let%try package = getPackage(uri, state);
      let%try (file, extra) = State.fileForUri(state, ~package, uri);

      {
        let pos = Utils.cmtLocFromVscode(pos);
        let%opt (location, loc) = References.locForPos(~extra, pos);
        let%opt text =
          Hover.newHover(
            ~rootUri=state.rootUri,
            ~file,
            ~getModule=State.fileForModule(state, ~package),
            ~markdown=!state.settings.clientNeedsPlainText,
            ~showPath=state.settings.showModulePathOnHover,
            loc,
          );
        Some(
          Ok((
            state,
            J.o([
              ("range", Protocol.rangeOfLoc(location)),
              (
                "contents",
                text
                |> Protocol.contentKind(!state.settings.clientNeedsPlainText),
              ),
            ]),
          )),
        );
      }
      |? Ok((state, J.null));
    },
  ),
  (
    "textDocument/rangeFormatting",
    (state, params) => {
      open InfixResult;
      let%try uri =
        params
        |> RJson.get("textDocument")
        |?> RJson.get("uri")
        |?> RJson.string;
      let%try package = getPackage(uri, state);
      let%try (start, end_) =
        RJson.get("range", params) |?> Protocol.rgetRange;

      let text = State.getContents(uri, state);
      let maybeResult = {
        let%try startPos =
          PartialParser.positionToOffset(text, start)
          |> orError("Invalid start position");
        let%try endPos =
          PartialParser.positionToOffset(text, end_)
          |> orError("Invalid end position");

        /** TODO: instead of bailing, it should extend the selection to encompass the whole line, and then go for it. */
        (
          if (fst(start) == fst(end_) && text.[endPos] != '\n') {
            Ok((state, J.null));
          } else {
            let substring = String.sub(text, startPos, endPos - startPos);
            open Utils;
            let trailingNewlines = substring |> countTrailing('\n');
            let (leadingNewlines, charsToFirstLines) = {
              let splitted = substring |> split_on_char('\n');
              let rec loop = (i, leadingLines, skipChars) => {
                let line = List.nth(splitted, i);
                switch (line |> String.trim |> String.length) {
                | 0 =>
                  loop(
                    i + 1,
                    leadingLines + 1,
                    skipChars + (line |> String.length),
                  )
                | _ => (leadingLines, skipChars + 1)
                };
              };
              loop(0, 0, 0);
            };

            /* Strip all leading new lines from substring */
            let (startPos, substring) =
              if (leadingNewlines > 0) {
                (
                  startPos + leadingNewlines,
                  String.sub(
                    substring,
                    charsToFirstLines,
                    String.length(substring) - charsToFirstLines,
                  ),
                );
              } else {
                (startPos, substring);
              };

            let indent =
              getFullLineOfPos(startPos, text) |> countLeading(' ');
            let cursorToFirstLineSpaces = substring |> countLeading(' ');

            let appendIndent = (~firstLineSpaces, indent, s) => {
              let indentString = repeat(indent, " ");
              if (indent == 0) {
                s;
              } else {
                split_on_char('\n', s)
                |> List.mapi((index, line) =>
                     switch (index, String.length(line)) {
                     | (_, 0) => line
                     | (0, _) => repeat(firstLineSpaces, " ") ++ line
                     | _ => indentString ++ line
                     }
                   )
                |> String.concat("\n");
              };
            };
            let%try fmtCmd =
              State.fmtCmdForUri(
                ~formatWidth=state.settings.formatWidth,
                ~interface=Utils.endsWith(uri, "i"),
                uri,
                package,
              );
            let%try_wrap text = AsYouType.format(substring, fmtCmd);
            (
              state,
              J.l([
                J.o([
                  ("range", Infix.(|!)(Json.get("range", params), "what")),
                  (
                    "newText",
                    J.s(
                      repeat(leadingNewlines, "\n")
                      ++ appendIndent(
                           ~firstLineSpaces=cursorToFirstLineSpaces,
                           indent,
                           text,
                         )
                      ++ repeat(trailingNewlines, "\n"),
                    ),
                  ),
                ]),
              ]),
            );
          }
        );
      };
      maybeResult;
    },
  ),
  (
    "textDocument/documentSymbol",
    (state, params) => {
      open InfixResult;
      let%try uri =
        params
        |> RJson.get("textDocument")
        |?> RJson.get("uri")
        |?> RJson.string;
      let%try package = getPackage(uri, state);

      let%try (file, _extra) = State.fileForUri(state, ~package, uri);
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
    },
  ),
  (
    "textDocument/formatting",
    (state, params) => {
      open InfixResult;
      let%try uri =
        params
        |> RJson.get("textDocument")
        |?> RJson.get("uri")
        |?> RJson.string;
      let%try package = getPackage(uri, state);
      let text = State.getContents(uri, state);
      let%try fmtCmd =
        State.fmtCmdForUri(
          ~formatWidth=state.settings.formatWidth,
          ~interface=Utils.endsWith(uri, "i"),
          uri,
          package,
        );
      let%try_wrap newText = AsYouType.format(text, fmtCmd);
      (
        state,
        text == newText
          ? Json.Null
          : J.l([
              J.o([
                (
                  "range",
                  Protocol.rangeOfInts(
                    0,
                    0,
                    List.length(Str.split(Str.regexp_string("\n"), text))
                    + 1,
                    0,
                  ),
                ),
                ("newText", J.s(newText)),
              ]),
            ]),
      );
    },
  ),
  (
    "textDocument/codeAction",
    (state, params) => {
      open InfixResult;
      let%try uri =
        RJson.get("textDocument", params)
        |?> RJson.get("uri")
        |?> RJson.string;
      let%try (start, _end) =
        RJson.get("range", params) |?> Protocol.rgetRange;
      let pos = start;
      let%try package = getPackage(uri, state);
      let%try (file, extra) = State.fileForUri(state, ~package, uri);
      Infix.(
        {
          let%opt () = Filename.check_suffix(uri, ".re") ? Some() : None;

          let pos = Utils.cmtLocFromVscode(pos);
          let%opt (_location, loc) = References.locForPos(~extra, pos);
          let%opt signatureText =
            switch (loc) {
            | Typed(t, Definition(stamp, Value)) =>
              let%opt declared =
                Query.declaredForTip(~stamps=file.stamps, stamp, Value);
              let%opt () =
                switch (declared.modulePath) {
                | File(_, _) => Some()
                | _ => None
                };
              let text =
                "let "
                ++ declared.name.txt
                ++ ": "
                ++ (t |> Shared.typeToString);
              Some(text);
            | TypeDefinition(name, decl, _) =>
              Some(decl |> Shared.declToString(name))
            | _ => None
            };
          Some(
            Ok((
              state,
              J.l([
                J.o([
                  ("title", J.s("Add to interface file")),
                  (
                    "command",
                    J.s("reason-language-server.add_to_interface_inner"),
                  ),
                  ("arguments", J.l([J.s(uri), J.s(signatureText)])),
                ]),
              ]),
            )),
          );
        }
        |? Ok((state, Json.Null))
      );
    },
  ),
  (
    "workspace/executeCommand",
    (state, params) => {
      open InfixResult;
      let%try command = RJson.get("command", params) |?> RJson.string;
      let%try arguments = RJson.get("arguments", params) |?> RJson.array;
      switch (command) {
      | "reason-language-server.add_to_interface_inner" =>
        switch (arguments) {
        | [uri, signatureText] =>
          let%try uri = RJson.string(uri);
          let%try signatureText = RJson.string(signatureText);
          let%try path =
            Utils.parseUri(uri) |> RResult.orError("Invalid uri");
          let interfacePath = path ++ "i";
          let interfaceUri = uri ++ "i";
          switch (Hashtbl.find_opt(state.documentText, interfaceUri)) {
          | None =>
            let text =
              switch (Files.readFileResult(interfacePath)) {
              | Ok(text) => text
              | Error(_) => ""
              };
            let%try () =
              Files.writeFileResult(
                interfacePath,
                text ++ "\n\n" ++ signatureText,
              );
            Ok((state, Json.Null));
          | Some((text, _, _)) =>
            let offset = String.length(text);
            let%try (line, col) =
              PartialParser.offsetToPosition(text, offset)
              |> RResult.orError("Invalid offset");
            Rpc.sendRequest(
              Log.log,
              stdout,
              "workspace/applyEdit",
              J.o([
                ("label", J.s("Add item to interface")),
                (
                  "edit",
                  J.o([
                    (
                      "changes",
                      J.o([
                        (
                          interfaceUri,
                          J.l([
                            J.o([
                              (
                                "range",
                                Protocol.rangeOfInts(line, col, line, col),
                              ),
                              ("newText", J.s("\n\n" ++ signatureText)),
                            ]),
                          ]),
                        ),
                      ]),
                    ),
                  ]),
                ),
              ]),
            );
            Ok((state, Json.Null));
          };
        | _ => Error("Invalid arguments")
        }
      | _ => Error("Unexpected command " ++ command)
      };
    },
  ),
  (
    "custom:reasonLanguageServer/dumpFileData",
    (state, params) => {
      open InfixResult;
      let%try uri = RJson.get("uri", params) |?> RJson.string;
      let%try path = Utils.parseUri(uri) |> RResult.orError("Invalid uri");
      let%try package = getPackage(uri, state);
      let interfacePath = path ++ ".dump.json";
      let%try text = State.getInterfaceFile(uri, state, ~package);
      let%try () = Files.writeFileResult(interfacePath, text);
      /* let interfaceUri = uri ++ "i"; */
      Ok((state, Json.Null));
    },
  ),
  (
    "custom:reasonLanguageServer/createInterface",
    (state, params) => {
      open InfixResult;
      let%try uri = RJson.get("uri", params) |?> RJson.string;
      let%try path = Utils.parseUri(uri) |> RResult.orError("Invalid uri");
      let%try package = getPackage(uri, state);
      let interfacePath = path ++ "i";
      let%try text = State.getInterfaceFile(uri, state, ~package);
      let%try () = Files.writeFileResult(interfacePath, text);
      /* let interfaceUri = uri ++ "i"; */
      Ok((state, Json.Null));
    },
  ),
  (
    "custom:reasonLanguageServer/showAst",
    (state, params) => {
      let%try (uri, _pos) = Protocol.rPositionParams(params);
      let%try package = getPackage(uri, state);
      let%try (file, _extra) = State.fileForUri(state, ~package, uri);
      let%try parsetree =
        AsYouType.getParsetree(
          ~uri,
          ~moduleName=file.moduleName,
          ~cacheLocation=package.tmpPath,
        );
      switch (parsetree) {
      | `Implementation(str) =>
        Printast.implementation(Format.str_formatter, str)
      | `Interface(int) => Printast.interface(Format.str_formatter, int)
      };
      Ok((state, Json.String(Format.flush_str_formatter())));
    },
  ),
];
