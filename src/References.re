open SharedTypes;

let debugReferences = ref(true);

let maybeLog = m =>
  if (debugReferences^) {
    Log.log("[ref] " ++ m);
  };

let checkPos =
    (
      (line, char),
      {Location.loc_start: {pos_lnum, pos_bol, pos_cnum}, loc_end},
    ) =>
  if (line < pos_lnum || line == pos_lnum && char < pos_cnum - pos_bol) {
    false;
  } else if (line > loc_end.pos_lnum
             || line == loc_end.pos_lnum
             && char > loc_end.pos_cnum
             - loc_end.pos_bol) {
    false;
  } else {
    true;
  };

let locsForPos = (~extra, pos) => {
  extra.locations |> List.filter(((loc, _l)) => checkPos(pos, loc));
};

let locForPos = (~extra, pos) => {
  switch (locsForPos(~extra, pos)) {
  | [
      (loc1, Typed(_, LocalReference(_))),
      (
        loc2,
        Typed(_, GlobalReference("Js_OO", Tip("unsafe_downgrade"), _)),
      ),
      (loc3, _) as l3,
    ]
      when loc1 == loc2 && loc2 == loc3 =>
    // JSX and compiler combined:
    // ~x becomes Js_OO.unsafe_downgrade(Props)#x
    // heuristic for: [Props, unsafe_downgrade, x], give loc of `x`
    Some(l3)
  | [(loc1, _), (loc2, _) as l, (loc3, _)]
      when loc1 == loc2 && loc2 == loc3 =>
    // JSX with at most one child
    // heuristic for: [makeProps, make, createElement], give the loc of `make`
    Some(l)
  | [(loc1, _), (loc2, _), (loc3, _) as l, (loc4, _)]
      when loc1 == loc2 && loc2 == loc3 && loc3 == loc4 =>
    // JSX variadic, e.g. <C> {x} {y} </C>
    // heuristic for: [makeProps, React.null, make, createElementVariadic], give the loc of `make`
    Some(l)
  | [l, ..._] => Some(l)
  | _ => None
  };
};

/** Other locations *within this file* that refer to the same thing.
 *
 * Useful for "highlight" stuff. */
let localReferencesForLoc = (~file, ~extra, loc) =>
  switch (loc) {
  | Explanation(_)
  | Typed(_, NotFound)
  | LModule(NotFound)
  | TopLevelModule(_)
  | Constant(_) => None
  | TypeDefinition(_, _, stamp) =>
    Hashtbl.find_opt(extra.internalReferences, stamp)
  | LModule(LocalReference(stamp, tip) | Definition(stamp, tip))
  | Typed(_, LocalReference(stamp, tip) | Definition(stamp, tip)) =>
    open Infix;
    switch (
      switch (tip) {
      | Constructor(name) =>
        Query.getConstructor(file, stamp, name) |?>> (x => x.stamp)
      | Field(name) => Query.getField(file, stamp, name) |?>> (x => x.stamp)
      | _ => Some(stamp)
      }
    ) {
    | None => None
    | Some(localStamp) =>
      Hashtbl.find_opt(extra.internalReferences, localStamp)
    }
  | LModule(GlobalReference(moduleName, path, tip))
  | Typed(_, GlobalReference(moduleName, path, tip)) =>
    switch (Hashtbl.find_opt(extra.externalReferences, moduleName)) {
    | None => None
    | Some(refs) =>
      Some(
        refs
        |> Utils.filterMap(((p, t, l)) =>
             p == path && t == tip ? Some(l) : None
           ),
      )
    }
  };

let definedForLoc = (~file, ~getModule, locKind) => {
  let inner = (~file, stamp, tip) => {
    switch (tip) {
    | Constructor(name) =>
      switch (Query.getConstructor(file, stamp, name)) {
      | None => None
      | Some(constructor) => Some(([], `Constructor(constructor)))
      }
    | Field(name) =>
      switch (Query.getField(file, stamp, name)) {
      | None => None
      | Some(field) => Some(([], `Field(field)))
      }
    | _ =>
      maybeLog(
        "Trying for declared "
        ++ tipToString(tip)
        ++ " "
        ++ string_of_int(stamp)
        ++ " in file "
        ++ Uri2.toString(file.uri),
      );
      switch (Query.declaredForTip(~stamps=file.stamps, stamp, tip)) {
      | None => None
      | Some(declared) => Some((declared.docstring, `Declared))
      };
    };
  };

  switch (locKind) {
  | NotFound => None
  | LocalReference(stamp, tip)
  | Definition(stamp, tip) => inner(~file, stamp, tip)
  | GlobalReference(moduleName, path, tip) =>
    {
      maybeLog("Getting global " ++ moduleName);
      switch (
        getModule(moduleName)
        |> RResult.orError("Cannot get module " ++ moduleName)
      ) {
      | Error(e) => Error(e)
      | Ok(file) =>
        let env = Query.fileEnv(file);
        switch (
          Query.resolvePath(~env, ~path, ~getModule)
          |> RResult.orError("Cannot resolve path " ++ pathToString(path))
        ) {
        | Error(e) => Error(e)
        | Ok((env, name)) =>
          switch (
            Query.exportedForTip(~env, name, tip)
            |> RResult.orError(
                 "Exported not found for tip "
                 ++ name
                 ++ " > "
                 ++ tipToString(tip),
               )
          ) {
          | Error(e) => Error(e)
          | Ok(stamp) =>
            maybeLog(
              "Getting for " ++ string_of_int(stamp) ++ " in " ++ name,
            );
            switch (
              inner(~file=env.file, stamp, tip)
              |> RResult.orError("could not get defined")
            ) {
            | Error(e) => Error(e)
            | Ok(res) =>
              maybeLog("Yes!! got it");
              Ok(res);
            };
          }
        };
      };
    }
    |> RResult.toOptionAndLog
  };
};

let alternateDeclared = (~file, ~pathsForModule, ~getUri, declared, tip) => {
  switch (Hashtbl.find_opt(pathsForModule, file.moduleName)) {
  | None => None
  | Some(paths) =>
    maybeLog("paths for " ++ file.moduleName);
    switch (paths) {
    | IntfAndImpl(_, intf, _, impl) =>
      maybeLog("Have both!!");
      let intfUri = Uri2.fromPath(intf);
      let implUri = Uri2.fromPath(impl);
      if (intfUri == file.uri) {
        switch (getUri(implUri) |> RResult.toOptionAndLog) {
        | None => None
        | Some((file, extra)) =>
          switch (
            Query.declaredForExportedTip(
              ~stamps=file.stamps,
              ~exported=file.contents.exported,
              declared.name.txt,
              tip,
            )
          ) {
          | None => None
          | Some(declared) => Some((file, extra, declared))
          }
        };
      } else {
        switch (getUri(intfUri) |> RResult.toOptionAndLog) {
        | None => None
        | Some((file, extra)) =>
          switch (
            Query.declaredForExportedTip(
              ~stamps=file.stamps,
              ~exported=file.contents.exported,
              declared.name.txt,
              tip,
            )
          ) {
          | None => None
          | Some(declared) => Some((file, extra, declared))
          }
        };
      };
    | _ => None
    };
  };
};

let resolveModuleReference =
    (~file, ~getModule, declared: declared(moduleKind)) => {
  switch (declared.item) {
  | Structure(_) => Some((file, Some(declared)))
  | Ident(path) =>
    let env = Query.fileEnv(file);
    switch (Query.fromCompilerPath(~env, path)) {
    | `Not_found => None
    | `Exported(env, name) =>
      switch (Hashtbl.find_opt(env.exported.modules, name)) {
      | None => None
      | Some(stamp) =>
        switch (Hashtbl.find_opt(env.file.stamps.modules, stamp)) {
        | None => None
        | Some(md) =>
          Some((env.file, Some(md)))
          /* Some((env.file.uri, validateLoc(md.name.loc, md.extentLoc))) */
        }
      }
    | `Global(moduleName, path) =>
      switch (getModule(moduleName)) {
      | None => None
      | Some(file) =>
        let env = Query.fileEnv(file);
        switch (Query.resolvePath(~env, ~getModule, ~path)) {
        | None => None
        | Some((env, name)) =>
          switch (Hashtbl.find_opt(env.exported.modules, name)) {
          | None => None
          | Some(stamp) =>
            switch (Hashtbl.find_opt(env.file.stamps.modules, stamp)) {
            | None => None
            | Some(md) =>
              Some((env.file, Some(md)))
              /* Some((env.file.uri, validateLoc(md.name.loc, md.extentLoc))) */
            }
          }
        };
      }
    | `Stamp(stamp) =>
      switch (Hashtbl.find_opt(file.stamps.modules, stamp)) {
      | None => None
      | Some(md) =>
        Some((file, Some(md)))
        /* Some((file.uri, validateLoc(md.name.loc, md.extentLoc))) */
      }
    | `GlobalMod(name) =>
      switch (getModule(name)) {
      | None => None
      | Some(file) =>
        /* maybeLog("Congrats, found a global mod"); */
        Some((file, None))
      }
    | _ => None
    };
  };
};

let forLocalStamp =
    (
      ~pathsForModule,
      ~file,
      ~extra,
      ~allModules,
      ~getModule,
      ~getUri,
      ~getExtra,
      stamp,
      tip,
    ) => {
  let env = Query.fileEnv(file);
  open Infix;
  switch (
    switch (tip) {
    | Constructor(name) =>
      Query.getConstructor(file, stamp, name) |?>> (x => x.stamp)
    | Field(name) => Query.getField(file, stamp, name) |?>> (x => x.stamp)
    | _ => Some(stamp)
    }
  ) {
  | None => None
  | Some(localStamp) =>
    switch (Hashtbl.find_opt(extra.internalReferences, localStamp)) {
    | None => None
    | Some(local) =>
      maybeLog("Checking externals: " ++ string_of_int(stamp));
      let externals =
        switch (Query.declaredForTip(~stamps=env.file.stamps, stamp, tip)) {
        | None => []
        | Some(declared) =>
          if (isVisible(declared)) {
            /**
              if this file has a corresponding interface or implementation file
              also find the references in that file.
              */
            let alternativeReferences =
              switch (
                alternateDeclared(
                  ~pathsForModule,
                  ~file,
                  ~getUri,
                  declared,
                  tip,
                )
              ) {
              | None => []
              | Some((file, extra, {stamp})) =>
                switch (
                  switch (tip) {
                  | Constructor(name) =>
                    Query.getConstructor(file, stamp, name) |?>> (x => x.stamp)
                  | Field(name) =>
                    Query.getField(file, stamp, name) |?>> (x => x.stamp)
                  | _ => Some(stamp)
                  }
                ) {
                | None => []
                | Some(localStamp) =>
                  switch (Hashtbl.find_opt(extra.internalReferences, localStamp)) {
                  | None => []
                  | Some(local) => [(file.uri, local)]
                  }
                }
              };
            switch (
              pathFromVisibility(declared.modulePath, declared.name.txt)
            ) {
            | None => []
            | Some(path) =>
              maybeLog("Now checking path " ++ pathToString(path));
              let thisModuleName = file.moduleName;
              let externals =
                allModules
                |> List.filter(name => name != file.moduleName)
                |> Utils.filterMap(name =>
                  {
                    switch (
                      getModule(name)
                      |> RResult.orError(
                          "Could not get file for module " ++ name,
                        )
                    ) {
                    | Error(e) => Error(e)
                    | Ok(file) =>
                      switch (
                        getExtra(name)
                        |> RResult.orError(
                            "Could not get extra for module " ++ name,
                          )
                      ) {
                      | Error(e) => Error(e)
                      | Ok(extra) =>
                        switch (
                          Hashtbl.find_opt(extra.externalReferences, thisModuleName)
                          |> RResult.orError(
                              "No references in "
                              ++ name
                              ++ " for "
                              ++ thisModuleName,
                            )
                        ) {
                        | Error(e) => Error(e)
                        | Ok(refs) =>
                          let refs =
                            refs
                            |> Utils.filterMap(((p, t, l)) =>
                                p == path && t == tip ? Some(l) : None
                              );
                          Ok((file.uri, refs));
                        }
                      }
                    }
                  } |> RResult.toOptionAndLog
              );
              alternativeReferences @ externals;
            };
          } else {
            maybeLog("Not visible");
            [];
          }
        };
      Some([(file.uri, local), ...externals]);
    }
  };
};

let allReferencesForLoc =
    (
      ~pathsForModule,
      ~getUri,
      ~file,
      ~extra,
      ~allModules,
      ~getModule,
      ~getExtra,
      loc,
    ) => {
  switch (loc) {
  | Explanation(_)
  | Typed(_, NotFound)
  | LModule(NotFound)
  | TopLevelModule(_)
  | Constant(_) => Error("Not a valid loc")
  | TypeDefinition(_, _, stamp) =>
    forLocalStamp(
      ~pathsForModule,
      ~getUri,
      ~file,
      ~extra,
      ~allModules,
      ~getModule,
      ~getExtra,
      stamp,
      Type,
    )
    |> RResult.orError("Could not get for local stamp")
  | Typed(_, LocalReference(stamp, tip) | Definition(stamp, tip))
  | LModule(LocalReference(stamp, tip) | Definition(stamp, tip)) =>
    maybeLog(
      "Finding references for "
      ++ Uri2.toString(file.uri)
      ++ " and stamp "
      ++ string_of_int(stamp)
      ++ " and tip "
      ++ tipToString(tip),
    );
    forLocalStamp(
      ~pathsForModule,
      ~getUri,
      ~file,
      ~extra,
      ~allModules,
      ~getModule,
      ~getExtra,
      stamp,
      tip,
    )
    |> RResult.orError("Could not get for local stamp");
  | LModule(GlobalReference(moduleName, path, tip))
  | Typed(_, GlobalReference(moduleName, path, tip)) =>
    switch (
      getModule(moduleName)
      |> RResult.orError("Cannot get module " ++ moduleName)
    ) {
    | Error(e) => Error(e)
    | Ok(file) =>
      let env = Query.fileEnv(file);
      switch (
        Query.resolvePath(~env, ~path, ~getModule)
        |> RResult.orError("Cannot resolve path " ++ pathToString(path))
      ) {
      | Error(e) => Error(e)
      | Ok((env, name)) =>
        switch (
          Query.exportedForTip(~env, name, tip)
          |> RResult.orError(
            "Exported not found for tip " ++ name ++ " > " ++ tipToString(tip)
          )
        ) {
        | Error(e) => Error(e)
        | Ok(stamp) =>
          switch (getUri(env.file.uri)) {
          | Error(e) => Error(e)
          | Ok((file, extra)) =>
            maybeLog(
              "Finding references for (global) "
              ++ Uri2.toString(env.file.uri)
              ++ " and stamp "
              ++ string_of_int(stamp)
              ++ " and tip "
              ++ tipToString(tip),
            );
            forLocalStamp(
              ~pathsForModule,
              ~getUri,
              ~file,
              ~extra,
              ~allModules,
              ~getModule,
              ~getExtra,
              stamp,
              tip,
            )
            |> RResult.orError("Could not get for local stamp");
          }
        }
      };
    }
  };
};

let refsForPos = (~file, ~extra, pos) => {
  switch (locForPos(~extra, pos)) {
  | None => None
  | Some((_, loc)) =>
    maybeLog("Got a loc for pos");
    localReferencesForLoc(~file, ~extra, loc);
  }
};

let validateLoc = (loc: Location.t, backup: Location.t) =>
  if (loc.loc_start.pos_cnum == (-1)) {
    if (backup.loc_start.pos_cnum == (-1)) {
      {
        Location.loc_ghost: true,
        loc_start: {
          pos_cnum: 0,
          pos_lnum: 1,
          pos_bol: 0,
          pos_fname: "",
        },
        loc_end: {
          pos_cnum: 0,
          pos_lnum: 1,
          pos_bol: 0,
          pos_fname: "",
        },
      };
    } else {
      backup;
    };
  } else {
    loc;
  };

let resolveModuleDefinition = (~file, ~getModule, stamp) => {
  switch (Hashtbl.find_opt(file.stamps.modules, stamp)) {
  | None => None
  | Some(md) =>
    switch (resolveModuleReference(~file, ~getModule, md)) {
    | None => None
    | Some((file, declared)) =>
      let loc =
        switch (declared) {
        | None => Utils.topLoc(Uri2.toPath(file.uri))
        | Some(declared) =>
          validateLoc(declared.name.loc, declared.extentLoc)
        };
      Some((file.uri, loc));
    }
  };
};

let definition = (~file, ~getModule, stamp, tip) => {
  switch (tip) {
  | Constructor(name) =>
    switch (Query.getConstructor(file, stamp, name)) {
    | None => None
    | Some(constructor) => Some((file.uri, constructor.cname.loc))
    }
  | Field(name) =>
    switch (Query.getField(file, stamp, name)) {
    | None => None
    | Some(field) => Some((file.uri, field.fname.loc))
    }
  | Module => resolveModuleDefinition(~file, ~getModule, stamp)
  | _ =>
    switch (Query.declaredForTip(~stamps=file.stamps, stamp, tip)) {
    | None => None
    | Some(declared) =>
      let loc = validateLoc(declared.name.loc, declared.extentLoc);
      let env = Query.fileEnv(file);
      let uri = Query.getSourceUri(~env, ~getModule, declared.modulePath);
      maybeLog("Inner uri " ++ Uri2.toString(uri));
      Some((uri, loc));
    }
  };
};

let orLog = (message, v) =>
  switch (v) {
  | None =>
    maybeLog(message);
    None;
  | _ => v
  };

let definitionForLoc = (~pathsForModule, ~file, ~getUri, ~getModule, loc) => {
  switch (loc) {
  | Typed(_, Definition(stamp, tip)) =>
    maybeLog("Trying to find a defintion for a definition");
    switch (Query.declaredForTip(~stamps=file.stamps, stamp, tip)) {
    | None => None
    | Some(declared) =>
      maybeLog("Declared");
      if (declared.exported) {
        maybeLog("exported, looking for alternate " ++ file.moduleName);
        switch (
          alternateDeclared(~pathsForModule, ~file, ~getUri, declared, tip)
        ) {
        | None => None
        | Some((file, _extra, declared)) =>
          let loc = validateLoc(declared.name.loc, declared.extentLoc);
          Some((file.uri, loc));
        };
      } else {
        None;
      };
    };
  | Explanation(_)
  | Typed(_, NotFound)
  | LModule(NotFound | Definition(_, _))
  | TypeDefinition(_, _, _)
  | Constant(_) => None
  | TopLevelModule(name) =>
    maybeLog("Toplevel " ++ name);
    open Infix;
    switch (
      Hashtbl.find_opt(pathsForModule, name)
      |> orLog("No paths found")
      |?> getSrc
      |> orLog("No src found")
    ) {
    | None => None
    | Some(src) => Some((Uri2.fromPath(src), Utils.topLoc(src)))
    }
  | LModule(LocalReference(stamp, tip))
  | Typed(_, LocalReference(stamp, tip)) =>
    maybeLog("Local defn " ++ tipToString(tip));
    definition(~file, ~getModule, stamp, tip);
  | LModule(GlobalReference(moduleName, path, tip))
  | Typed(_, GlobalReference(moduleName, path, tip)) =>
    maybeLog(
      "Global defn "
      ++ moduleName
      ++ " "
      ++ pathToString(path)
      ++ " : "
      ++ tipToString(tip),
    );
    switch (getModule(moduleName)) {
    | None => None
    | Some(file) =>
      let env = Query.fileEnv(file);
      switch (Query.resolvePath(~env, ~path, ~getModule)) {
      | None => None
      | Some((env, name)) =>
        switch (Query.exportedForTip(~env, name, tip)) {
        | None => None
        | Some(stamp) =>
          /** oooh wht do I do if the stamp is inside a pseudo-file? */
          maybeLog("Got stamp " ++ string_of_int(stamp));
          definition(~file=env.file, ~getModule, stamp, tip);
        }
      };
    };
  };
};

let definitionForPos =
    (~pathsForModule, ~file, ~extra, ~getUri, ~getModule, pos) =>
  switch (locForPos(~extra, pos)) {
  | None => None
  | Some((_, loc)) =>
    maybeLog("Got a loc for pos");
    definitionForLoc(~pathsForModule, ~file, ~getUri, ~getModule, loc);
  };
