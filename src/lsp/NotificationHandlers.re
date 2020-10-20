open Infix;
open RResult;
open TopTypes;

let recompileDebounceTime = 0.5; /* seconds */

let getTextDocument = doc => {
  let%opt uri = Json.get("uri", doc) |?> Json.string;
  let%opt version = Json.get("version", doc) |?> Json.number;
  let%opt text = Json.get("text", doc) |?> Json.string;
  Some((uri, version, text));
};

let reportDiagnostics = (uri, result) => {
  open JsonShort;
  let body =
    switch (result) {
    | `BuildFailed(lines) =>
      o([
        ("uri", s(uri)),
        (
          "diagnostics",
          l([
            o([
              ("range", Protocol.rangeOfInts(0, 0, 5, 0)),
              ("message", s(Utils.stripAnsii(String.concat("\n", lines)))),
              ("severity", i(1)),
            ]),
          ]),
        ),
      ])
    | `BuildSucceeded => o([("uri", s(uri)), ("diagnostics", l([]))])
    };
  Rpc.sendNotification(stdout, "textDocument/publishDiagnostics", body);
};

let checkPackageTimers = state => {
  Hashtbl.iter(
    (_, package) =>
      if (package.rebuildTimer != 0.
          && package.rebuildTimer < Unix.gettimeofday()) {
        package.rebuildTimer = 0.;
        // TODO report the error here
        ignore @@
        BuildCommand.runBuildCommand(
          ~reportDiagnostics,
          ~state,
          ~rootPath=package.rootPath,
          package.buildCommand,
        );
      },
    state.packagesByRoot,
  );
};

let setPackageTimer = package =>
  if (package.rebuildTimer == 0.) {
    package.rebuildTimer = Unix.gettimeofday() +. 0.01;
  };

let watchedFileContentsMap = Hashtbl.create(100);

let reloadAllState = state => {
  Log.log("RELOADING ALL STATE");
  Hashtbl.iter(
    (uri, _) =>
      Hashtbl.replace(
        state.documentTimers,
        uri,
        Unix.gettimeofday() +. recompileDebounceTime,
      ),
    state.documentText,
  );
  {
    ...TopTypes.empty(),
    documentText: state.documentText,
    documentTimers: state.documentTimers,
    settings: state.settings,
  };
};

let dumpLocations = (state, ~package, ~file, ~extra, uri) => {
  let locations =
    extra.SharedTypes.locations
    |> List.filter(((l, _)) => !l.Location.loc_ghost);
  Log.log(
    "ZZZ found "
    ++ string_of_int(List.length(locations))
    ++ " locations in "
    ++ uri,
  );
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
             ~rootUri=state.rootUri,
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
  Log.spamError := true;
  Log.log("ZZZ " ++ Json.stringify(locationsInfo));
  Log.spamError := false;
};

let notificationHandlers:
  list((string, (state, Json.t) => result(state, string))) = [
  (
    "textDocument/didOpen",
    (state, params) => {
      let%try (uri, version, text) =
        Json.get("textDocument", params)
        |?> getTextDocument
        |> RResult.orError("Invalid params");
      Hashtbl.replace(
        state.documentText,
        uri,
        (text, int_of_float(version), true),
      );
      Hashtbl.replace(
        state.documentTimers,
        uri,
        Unix.gettimeofday() +. recompileDebounceTime,
      );

      let%try path = Utils.parseUri(uri) |> RResult.orError("Invalid uri");
      if (FindFiles.isSourceFile(path)) {
        let%try package = Packages.getPackage(~reportDiagnostics, uri, state);
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
    (state, params) => {
      let nullIfEmpty = item => item == "" ? None : Some(item);
      let settings =
        params |> Json.get("settings") |?> Json.get("reason_language_server");
      let mlfmtLocation =
        settings |?> Json.get("mlfmt") |?> Json.string |?> nullIfEmpty;
      let refmtLocation =
        settings |?> Json.get("refmt") |?> Json.string |?> nullIfEmpty;
      let lispRefmtLocation =
        settings |?> Json.get("lispRefmt") |?> Json.string |?> nullIfEmpty;
      let perValueCodelens =
        settings |?> Json.get("per_value_codelens") |?> Json.bool |? false;
      let opensCodelens =
        settings |?> Json.get("opens_codelens") |?> Json.bool |? true;
      let dependenciesCodelens =
        settings |?> Json.get("dependencies_codelens") |?> Json.bool |? true;
      let formatWidth =
        settings
        |?> Json.get("format_width")
        |?> Json.number
        |?>> int_of_float;
      let showModulePathOnHover =
        settings
        |?> Json.get("show_module_path_on_hover")
        |?> Json.bool
        |? true;
      let autoRebuild =
        settings |?> Json.get("autoRebuild") |?> Json.bool |? true;

      Ok({
        ...state,
        settings: {
          ...state.settings,
          perValueCodelens,
          mlfmtLocation,
          refmtLocation,
          lispRefmtLocation,
          opensCodelens,
          formatWidth,
          dependenciesCodelens,
          showModulePathOnHover,
          autoRebuild,
        },
      });
    },
  ),
  (
    "textDocument/didSave",
    (state, params) => {
      open InfixResult;
      let%try uri =
        params
        |> RJson.get("textDocument")
        |?> (doc => RJson.get("uri", doc) |?> RJson.string);
      let%try package = Packages.getPackage(~reportDiagnostics, uri, state);
      setPackageTimer(package);
      let moduleName = FindFiles.getName(uri);
      package.localModules
      |> List.iter(mname => {
           let%opt_consume paths =
             Hashtbl.find_opt(package.pathsForModule, mname);
           let%opt_consume src = SharedTypes.getSrc(paths);
           let otherUri = Utils.toUri(src);
           let refs =
             Hashtbl.find_opt(package.interModuleDependencies, mname);
           Infix.(
             if (mname != moduleName
                 && (
                   List.mem(moduleName, refs |? [])
                   || (
                     switch (Hashtbl.find(state.compiledDocuments, otherUri)) {
                     | exception Not_found => true
                     | Success(_) => false
                     | SyntaxError(_) => false
                     | TypeError(_) => true
                     }
                   )
                 )) {
               Hashtbl.remove(state.compiledDocuments, otherUri);
               Hashtbl.replace(
                 state.documentTimers,
                 otherUri,
                 Unix.gettimeofday() +. 0.015,
               );
             }
           );
         });

      Ok(state);
    },
  ),
  (
    "textDocument/didChange",
    (state, params) => {
      open InfixResult;
      let%try doc = params |> RJson.get("textDocument");
      let%try uri = RJson.get("uri", doc) |?> RJson.string;
      let%try version = RJson.get("version", doc) |?> RJson.number;
      let%try changes = RJson.get("contentChanges", params) |?> RJson.array;
      let%try text =
        List.nth(changes, List.length(changes) - 1)
        |> RJson.get("text")
        |?> RJson.string;
      /* Hmm how do I know if it's modified? */
      let state = State.updateContents(uri, text, version, state);
      Hashtbl.replace(
        state.documentTimers,
        uri,
        Unix.gettimeofday() +. recompileDebounceTime,
      );
      Ok(state);
    },
  ),
  (
    "workspace/didChangeWatchedFiles",
    (state, params) => {
      Log.log("Got a watched file change");
      let%try changes = RJson.get("changes", params);
      let%try changes = RJson.array(changes);
      open InfixResult;
      let shouldReload =
        changes
        |> List.exists(change =>
             {
               let%try uri = RJson.get("uri", change) |?> RJson.string;
               let isRelevant = Utils.endsWith(uri, "/bsconfig.json");
               if (!isRelevant) {
                 Ok(false);
               } else {
                 let%try path =
                   Utils.parseUri(uri) |> RResult.orError("Cannot parse URI");
                 let%try contents = Files.readFileResult(path);
                 if (!Hashtbl.mem(watchedFileContentsMap, uri)
                     || Hashtbl.find(watchedFileContentsMap, uri) == contents) {
                   Ok(false);
                 } else {
                   Hashtbl.replace(watchedFileContentsMap, uri, contents);
                   Log.log("Reloading because a file changed: " ++ uri);
                   Ok(true);
                 };
               };
             }
             |? false
           );

      if (shouldReload) {
        Ok(reloadAllState(state));
      } else {
        Ok(state);
      };
    },
  ),
];
