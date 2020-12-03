open Infix;

let dumpLocations = (state, ~package, ~file, ~extra, ~selectPos, uri) => {
  let locations =
    extra.SharedTypes.locations
    |> List.filter(((l, _)) => !l.Location.loc_ghost);
  let locations = {
    switch (selectPos) {
    | Some(pos) =>
      let pos = Utils.cmtLocFromVscode(pos);
      switch (References.locForPos(~extra={...extra, locations}, pos)) {
      | None => []
      | Some(l) => [l]
      };
    | None => locations
    };
  };
  open JsonShort;
  let dedupTable = Hashtbl.create(1);
  let dedupHover = (hover, i) => {
    let isCandidate = String.length(hover) > 10;
    if (isCandidate) {
      switch (Hashtbl.find_opt(dedupTable, hover)) {
      | Some(n) => s("#" ++ string_of_int(n))
      | None =>
        Hashtbl.replace(dedupTable, hover, i);
        s(hover);
      };
    } else {
      s(hover);
    };
  };
  let locationsInfo =
    locations
    |> Utils.filterMapIndex((i, (location: Location.t, loc)) => {
         let locIsModule =
           switch (loc) {
           | SharedTypes.LModule(_)
           | TopLevelModule(_) => true
           | TypeDefinition(_)
           | Typed(_)
           | Constant(_)
           | Explanation(_) => false
           };

         let hoverText =
           Hover.newHover(
             ~rootUri=state.TopTypes.rootUri,
             ~file,
             ~getModule=State.fileForModule(state, ~package),
             ~markdown=!state.settings.clientNeedsPlainText,
             ~showPath=state.settings.showModulePathOnHover,
             loc,
           )
           |? "";
         let hover =
           hoverText == "" ? [] : [("hover", dedupHover(hoverText, i))];

         let uriLocOpt =
           References.definitionForLoc(
             ~pathsForModule=package.pathsForModule,
             ~file,
             ~getUri=State.fileForUri(state, ~package),
             ~getModule=State.fileForModule(state, ~package),
             loc,
           );
         let (def, skipZero) =
           switch (uriLocOpt) {
           | None => ([], false)
           | Some((uri2, loc)) =>
             let uriIsCurrentFile = uri == uri2;
             let posIsZero = ({Lexing.pos_lnum, pos_bol, pos_cnum}) =>
               pos_lnum == 1 && pos_cnum - pos_bol == 0;
             // Skip if range is all zero, unless it's a module
             let skipZero =
               !locIsModule
               && loc.loc_start
               |> posIsZero
               && loc.loc_end
               |> posIsZero;
             let range = ("range", Protocol.rangeOfLoc(loc));
             (
               [
                 (
                   "definition",
                   o(
                     uriIsCurrentFile
                       ? [range] : [("uri", Json.String(uri2)), range],
                   ),
                 ),
               ],
               skipZero,
             );
           };
         let skip = skipZero || hover == [] && def == [];
         skip
           ? None
           : Some(
               o([("range", Protocol.rangeOfLoc(location))] @ hover @ def),
             );
       })
    |> l;

  Json.stringify(locationsInfo);
};

// Split (line,char) from filepath:line:char
let splitLineChar = pathWithPos => {
  let mkPos = (line, char) =>
    Some((line |> int_of_string, char |> int_of_string));
  switch (pathWithPos |> String.split_on_char(':')) {
  | [filePath, line, char] => (filePath, mkPos(line, char))
  | [drive, rest, line, char] =>
    // c:\... on Windows
    (drive ++ ":" ++ rest, mkPos(line, char))
  | _ => (pathWithPos, None)
  };
};

let dump = files => {
  Shared.cacheTypeToString := true;
  let rootPath = Unix.getcwd();
  let emptyState = TopTypes.empty();
  let state = {
    ...emptyState,
    rootPath,
    rootUri: Utils.toUri(rootPath),
    settings: {
      ...emptyState.settings,
      autoRebuild: false,
    },
  };
  files
  |> List.iter(pathWithPos => {
       let (filePath, selectPos) = pathWithPos |> splitLineChar;
       let filePath = maybeConcat(Unix.getcwd(), filePath);
       let uri = Utils.toUri(filePath);
       let result =
         switch (State.getFullFromCmt(uri, state)) {
         | Error(message) =>
           prerr_endline(message);
           "[]";
         | Ok((package, {file, extra})) =>
           dumpLocations(state, ~package, ~file, ~extra, ~selectPos, uri)
         };
       print_endline(result);
     });
};

let autocomplete = (~currentFile, ~full, ~package, ~pos, ~state) => {
  let maybeText = Files.readFile(currentFile);
  let completions =
    NewCompletions.computeCompletions(
      ~full,
      ~maybeText,
      ~package,
      ~pos,
      ~state,
    );
  Json.stringify(completions);
};

let complete = (~pathWithPos, ~currentFile) => {
  let rootPath = Unix.getcwd();
  let emptyState = TopTypes.empty();
  let state = {
    ...emptyState,
    rootPath,
    rootUri: Utils.toUri(rootPath),
    settings: {
      ...emptyState.settings,
      autoRebuild: false,
    },
  };
  switch (pathWithPos |> splitLineChar) {
  | (filePath, Some(pos)) =>
    let filePath = maybeConcat(Unix.getcwd(), filePath);
    let uri = Utils.toUri(filePath);
    let result =
      switch (State.getFullFromCmt(uri, state)) {
      | Error(message) =>
        prerr_endline(message);
        "[]";
      | Ok((package, full)) =>
        Hashtbl.replace(state.lastDefinitions, uri, full);
        autocomplete(~currentFile, ~full, ~package, ~pos, ~state);
      };
    print_endline(result);
  | _ => ()
  };
};
