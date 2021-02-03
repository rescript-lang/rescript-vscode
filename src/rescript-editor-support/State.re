open Infix;

open TopTypes;

let isMl = path =>
  Filename.check_suffix(path, ".ml") || Filename.check_suffix(path, ".mli");

let odocToMd = text => MarkdownOfOCamldoc.convert(text);
let compose = (fn1, fn2, arg) => fn1(arg) |> fn2;

let converter = src => {
  let mlToOutput = compose(odocToMd, Omd.to_markdown);
  fold(src, mlToOutput, src => isMl(src) ? mlToOutput : (x => x));
};

let newDocsForCmt = (~moduleName, cmtCache, changed, cmt, src) => {
  let uri = Uri2.fromPath(src |? cmt);
  let%opt file =
    Process_406.fileForCmt(~moduleName, ~uri, cmt, converter(src))
    |> RResult.toOptionAndLog;
  Hashtbl.replace(cmtCache, cmt, (changed, file));
  Some(file);
};

let docsForCmt = (~moduleName, cmt, src, state) =>
  if (Hashtbl.mem(state.cmtCache, cmt)) {
    let (mtime, docs) = Hashtbl.find(state.cmtCache, cmt);
    /* TODO I should really throttle this mtime checking to like every 50 ms or so */
    switch (Files.getMtime(cmt)) {
    | None =>
      Log.log("⚠️ cannot get docs for nonexistant cmt " ++ cmt);
      None;
    | Some(changed) =>
      if (changed > mtime) {
        newDocsForCmt(~moduleName, state.cmtCache, changed, cmt, src);
      } else {
        Some(docs);
      }
    };
  } else {
    switch (Files.getMtime(cmt)) {
    | None =>
      Log.log("⚠️ cannot get docs for nonexistant cmt " ++ cmt);
      None;
    | Some(changed) =>
      newDocsForCmt(~moduleName, state.cmtCache, changed, cmt, src)
    };
  };

let updateContents = (uri, text, state) => {
  Hashtbl.replace(state.documentText, uri, text);
  state;
};

open Infix;

let getFullFromCmt = (~state, ~uri) => {
  let path = Uri2.toPath(uri);
  let%try package = Packages.getPackage(uri, state);
  let moduleName =
    BuildSystem.namespacedName(package.namespace, FindFiles.getName(path));
  switch (Hashtbl.find_opt(package.pathsForModule, moduleName)) {
  | Some(paths) =>
    let cmt =
      SharedTypes.getCmt(
        ~interface=Utils.endsWith(path, "i"),
        paths,
      );
    let%try full = Process_406.fullForCmt(~moduleName, ~uri, cmt, x => x);
    Hashtbl.replace(
      package.interModuleDependencies,
      moduleName,
      SharedTypes.hashList(full.extra.externalReferences) |> List.map(fst),
    );
    Ok((package, full));
  | None => Error("can't find module " ++ moduleName)
  };
};

let docsForModule = (modname, state, ~package) =>
  if (Hashtbl.mem(package.pathsForModule, modname)) {
    let paths = Hashtbl.find(package.pathsForModule, modname);
    /* TODO do better */
    let cmt = SharedTypes.getCmt(paths);
    let src = SharedTypes.getSrc(paths);
    Log.log("FINDING docs for module " ++ SharedTypes.showPaths(paths));
    Log.log("FINDING " ++ cmt ++ " src " ++ (src |? ""));
    switch (docsForCmt(~moduleName=modname, cmt, src, state)) {
    | None => None
    | Some(docs) => Some((docs, src))
    };
  } else {
    Log.log("No path for module " ++ modname);
    None;
  };

let fileForUri = (state, uri) => {
  let%try (_package, {extra, file}) = getFullFromCmt(~state, ~uri);
  Ok((file, extra));
};

let fileForModule = (state, ~package, modname) => {
  let%opt (file, _) = docsForModule(modname, state, ~package);
  Some(file);
};

let extraForModule = (state, ~package, modname) =>
  if (Hashtbl.mem(package.pathsForModule, modname)) {
    let paths = Hashtbl.find(package.pathsForModule, modname);
    /* TODO do better? */
    let%opt src = SharedTypes.getSrc(paths);
    switch (getFullFromCmt(~state, ~uri=Uri2.fromPath(src))) {
    | Ok((_package, {extra})) => Some(extra)
    | Error(_) => None
    };
  } else {
    None;
  };
