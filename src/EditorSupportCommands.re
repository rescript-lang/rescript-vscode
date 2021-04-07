module J = JsonShort;

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
  let dedupTable = Hashtbl.create(1);
  let dedupHover = (hover, i) => {
    let isCandidate = String.length(hover) > 10;
    if (isCandidate) {
      switch (Hashtbl.find_opt(dedupTable, hover)) {
      | Some(n) => J.s("#" ++ string_of_int(n))
      | None =>
        Hashtbl.replace(dedupTable, hover, i);
        J.s(hover);
      };
    } else {
      J.s(hover);
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
             ~file,
             ~getModule=State.fileForModule(state, ~package),
             loc,
           );
         let hover =
           switch (hoverText) {
           | None => []
           | Some(s) => [("hover", dedupHover(s, i))]
           };

         let uriLocOpt =
           References.definitionForLoc(
             ~pathsForModule=package.pathsForModule,
             ~file,
             ~getUri=State.fileForUri(state),
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
                   J.o(
                     uriIsCurrentFile
                       ? [range]
                       : [("uri", Json.String(Uri2.toString(uri2))), range],
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
               J.o(
                 [("range", Protocol.rangeOfLoc(location))] @ hover @ def,
               ),
             );
       })
    |> J.l;

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
  let state = TopTypes.empty();
  files
  |> List.iter(pathWithPos => {
       let (filePath, selectPos) = pathWithPos |> splitLineChar;
       let filePath = Files.maybeConcat(Unix.getcwd(), filePath);
       let uri = Uri2.fromPath(filePath);
       let result =
         switch (State.getFullFromCmt(~state, ~uri)) {
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
  let state = TopTypes.empty();
  switch (pathWithPos |> splitLineChar) {
  | (filePath, Some(pos)) =>
    let filePath = Files.maybeConcat(Unix.getcwd(), filePath);
    let uri = Uri2.fromPath(filePath);
    let result =
      switch (State.getFullFromCmt(~state, ~uri)) {
      | Error(message) =>
        prerr_endline(message);
        "[]";
      | Ok((package, full)) =>
        autocomplete(~currentFile, ~full, ~package, ~pos, ~state)
      };
    print_endline(result);
  | _ => ()
  };
};
