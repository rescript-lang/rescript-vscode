open Infix;

let debug = ref(false);

let maybeLog = message => if (debug^) { Log.log("[MerlinFile]: " ++ message) };

/** This is a dirty hack to get around the bug in bsb native that doesn't do the proper ppx flags for ppxs */
let fixPpxBsNative = (flg, base) => {
  switch (Str.split(Str.regexp_string(" "), flg)) {
    | ["-ppx", ppx] when Str.string_match(Str.regexp("[a-zA-Z_]+"), ppx, 0) && !Str.string_match(Str.regexp("[a-zA-Z_]:"), ppx, 0) => {
      "-ppx " ++ (base /+ "lib" /+ "bs" /+ "native" /+ String.lowercase_ascii(ppx) ++ ".native")
    }
    | _ => flg
  }
};

let parseMerlin = (text) => {
  let lines = Str.split(Str.regexp_string("\n"), text);
  List.fold_left(
    ((source, build, flags), line) => {
      if (Utils.startsWith(line, "FLG ")) {
        (source, build, [Utils.chopPrefix(line, "FLG "), ...flags])
      } else if (Utils.startsWith(line, "S ")) {
        ([Utils.chopPrefix(line, "S "), ...source], build, flags)
      } else if (Utils.startsWith(line, "B ")) {
        (source, [Utils.chopPrefix(line, "B "), ...build], flags)
      } else {
        (source, build, flags)
      }
    },
    ([], [], []),
    lines
  )
};

// let maybeConcat = (base, path) => Infix.isFullPath(path) ? path : Filename.concat(base, path);

let isRelativePath = Sys.win32
? path => !Str.string_match(Str.regexp("[A-Za-z]:"), path, 0)
: path => path != "" && path.[0] != '/';

let isBuildFile = name =>
  Filename.check_suffix(name, ".cmt")
  || Filename.check_suffix(name, ".cmti");

let isSourceFile = name =>
  Filename.check_suffix(name, ".re")
  || Filename.check_suffix(name, ".rei")
  || Filename.check_suffix(name, ".ml")
  || Filename.check_suffix(name, ".mli")
  || Filename.check_suffix(name, ".rel")
  || Filename.check_suffix(name, ".reli");

let rec tryReduce: 'a 'b 'e . (list('a), 'b, ('b, 'a) => result('b, 'e)) => result('b, 'e) = (items, current, fn) => {
  switch items {
    | [] => Ok(current)
    | [one, ...rest] => switch (fn(current, one)) {
      | Error(e) => Error(e)
      | Ok(v) => tryReduce(rest, v, fn)
    }
  }
};

let maybeHash = (h, k) => switch (Hashtbl.find(h, k)) {
  | exception Not_found => None
  | x => Some(x)
};

let (|??) = (a, b) => switch a {
  | None => b
  | Some(m) => Some(m)
};

let orLog = v => switch v {
  | Error(m) => 
    Log.log("[MerlinFile]: Error: " ++ m);
    None
  | Ok(m) => Some(m)
};

type files = {src: option(string), srci: option(string), cmt: option(string), cmti: option(string)};

let orBlank = v => switch v {
  | None => "()"
  | Some(m) => m
};

let showFiles = ({src, srci, cmt, cmti}) =>
  Printf.sprintf("src: %s, srci: %s, cmt: %s, cmti: %s",
  src->orBlank,
  srci->orBlank,
  cmt->orBlank,
  cmti->orBlank
  );

let showPaths = paths => switch paths {
  | `Impl(cmt, src) => Printf.sprintf("Impl(%s, %s)", cmt, src |? "nil")
  | `Intf(cmti, src) => Printf.sprintf("Intf(%s, %s)", cmti, src |? "nil")
  | `IntfAndImpl(cmti, srci, cmt, src) => Printf.sprintf("IntfAndImpl(%s, %s, %s, %s)", cmti, srci |? "nil", cmt, src |? "nil")
};

let calcPaths = (mname, files) => {
  switch (files) {
  | {src: None, srci: None, cmt: Some(cmt), cmti: Some(cmti)} =>
    Ok(`IntfAndImpl((cmti, None, cmt, None)))
  | {src: None, srci: None, cmt: None, cmti: Some(cmti)} => Ok(`Intf((cmti, None)))
  | {src: None, srci: None, cmt: Some(cmt), cmti: None} => Ok(`Impl((cmt, None)))
  | {src: None, srci: None} =>
    Error("No source files for module " ++ mname ++ " - " ++ showFiles(files))
  | {src: Some(src), srci: Some(srci), cmt: Some(cmt), cmti: Some(cmti)} =>
    Ok(`IntfAndImpl((cmti, Some(srci), cmt, Some(src))))
  | {src: Some(src), srci: None, cmt: Some(cmt)} => Ok(`Impl((cmt, Some(src))))
  | {src: None, srci: Some(srci), cmti: Some(cmti)} => Ok(`Intf((cmti, Some(srci))))
  | _ =>
    Error("Insufficient build files found for module " ++ mname ++ " - " ++ showFiles(files))
  };
};

let hashtblKeys = (tbl) =>
  Hashtbl.fold((key,_,l) => [key, ...l], tbl, []) |> List.rev;

/** Returns a `pathsForModule`, `nameForPath`, `localModules` and `dependencyModules` */
let getModulesFromMerlin = (~stdlibs, base, text) => {
  let (source, build, _flags) = parseMerlin(text);

  let source = stdlibs @ source;
  let build = stdlibs @ build;

  let (localSource, depSource) = source|>List.partition(isRelativePath);

  maybeLog(Printf.sprintf("Local %d, Deps %d for %s/.merlin", List.length(localSource), List.length(depSource), base));

  // let buildByBasename = Hashtbl.create(List.length(source) * 5);

  let cmtByModuleName = Hashtbl.create(100);
  let cmtiByModuleName = Hashtbl.create(100);

  let addAndCheck = (tbl, k, v) => {
    if (Hashtbl.mem(tbl, k)) {
      maybeLog("DUPLICATE " ++ k ++ " : new value " ++ v ++ " : old value " ++ Hashtbl.find(tbl, k))
    } else {
      maybeLog("Build file for " ++ k ++ " : " ++ v);
    };
    Hashtbl.replace(tbl, k, v)
  };

  build|>List.iter(buildDir => {
    let buildDir = maybeConcat(base, buildDir);
    maybeLog("## Build dir " ++ buildDir);
    Files.readDirectory(buildDir)
    |>List.filter(isBuildFile)
    |>List.iter(name => {
        let full = fileConcat(buildDir, name);
        // maybeLog("Build file " ++ full);
        let moduleName = name->String.capitalize_ascii->Filename.chop_extension;
        if (Filename.check_suffix(name, ".cmt")) {
          cmtByModuleName->addAndCheck(moduleName, full)
        } else if (Filename.check_suffix(name, ".cmti")) {
          cmtiByModuleName->addAndCheck(moduleName, full)
        };
        // if (buildByBasename->Hashtbl.mem(name)) {
        //   maybeLog(
        //     "DUPLICATE "
        //     ++ name
        //     ++ " : "
        //     ++ full
        //     ++ " > prev "
        //     ++ Hashtbl.find(buildByBasename, name),
        //   );
        // };
        // buildByBasename->Hashtbl.replace(name, full);
      })
  }
  );

  let pathsForModule = Hashtbl.create(30);
  let nameForPath = Hashtbl.create(30);

  let add = (name, paths) => switch paths {
    | `Intf(_, Some(path)) => Hashtbl.replace(nameForPath, path, name)
    | `Impl(_, Some(path)) => Hashtbl.replace(nameForPath, path, name)
    | `IntfAndImpl(_, intf, _, impl) =>
        intf |?< path => Hashtbl.replace(nameForPath, path, name);
        impl |?< path => Hashtbl.replace(nameForPath, path, name);
    | _ => ()
  };

  let localModuleNames = Hashtbl.create(10);
  let depsModuleNames = Hashtbl.create(10);

  depSource|>List.iter(dep => {
    maybeLog("For dependency dir " ++ dep);
    let allFiles = dep->Files.readDirectory;
    let prefix = allFiles |> List.fold_left((found, name) => {
      switch (found) {
        | Some(m) => Some(m)
        | None =>
          switch (Str.split(Str.regexp_string("__"), name)) {
            | [prefix, _name, ..._] =>
            maybeLog(" > prefix: " ++ prefix);
            Some(prefix)
            | _ => None
          }
      }
    }, None);
    let filesByName = Hashtbl.create(10);
    let moduleNames = Hashtbl.create(10);
    allFiles|>List.filter(isSourceFile)|>List.iter(file => {
      let full = dep /+ file;
      maybeLog(" > file " ++ full)
      filesByName->Hashtbl.replace(file, full)
      moduleNames->Hashtbl.replace(file->Filename.chop_extension, ())
    });
    moduleNames->hashtblKeys|>List.iter(mname => {
      let moduleName = switch prefix {
        | None => mname
        | Some(prefix) => prefix ++ "__" ++ (prefix == "stdlib" ? mname : mname->String.capitalize_ascii)
      } |> String.capitalize_ascii;
      // let moduleName = fullName->String.capitalize_ascii;
      let src = filesByName->maybeHash(mname ++ ".ml") |?? filesByName->maybeHash(mname ++ ".re");
      let srci = filesByName->maybeHash(mname ++ ".mli") |?? filesByName->maybeHash(mname ++ ".rei");
      let cmt = cmtByModuleName->maybeHash(moduleName);
      let cmti = cmtiByModuleName->maybeHash(moduleName);
      let (moduleName, cmt, cmti) = switch (cmt, cmti) {
        // TODO it would be nice not to have to do this fallback
        | (None, None) => (
          mname->String.capitalize_ascii,
          cmtByModuleName->maybeHash(mname->String.capitalize_ascii),
          cmtiByModuleName->maybeHash(mname->String.capitalize_ascii),
        )
        | _ => (moduleName, cmt, cmti)
      };
      // let fullName = fullName->String.capitalize_ascii;
      depsModuleNames->Hashtbl.replace(moduleName, ());
      let%opt_consume paths = calcPaths(moduleName, {src, srci, cmt, cmti}) |> orLog;
      maybeLog(" > module " ++ moduleName ++ " :: " ++ showPaths(paths));
      pathsForModule->Hashtbl.replace(moduleName, paths);
      add(moduleName, paths);
    })
  });

  // Go through the local source directories
  // for each one, look for a build directory that matches it
  // buut to do that I need to parse the dunefile, probably.
  // So:
  // Source directory, dunefile
  //   give me a list of `name, Library | Executable, modules`
  // Then I'll look for the compilation units that correspond.
  // Then for dependencies:
  // If there's a directory w/ both source & b in it, then process that
  // otherwise, :shrug:?
  // 
  // if there's a directory that directly matches, go with that.
  // Other issues: 

  // let add = (name, paths) => switch paths {
  //   | SharedTypes.Intf(_, Some(path)) => Hashtbl.replace(nameForPath, path, name)
  //   | SharedTypes.Impl(_, Some(path)) => Hashtbl.replace(nameForPath, path, name)
  //   | SharedTypes.IntfAndImpl(_, intf, _, impl) =>
  //       intf |?< path => Hashtbl.replace(nameForPath, path, name);
  //       impl |?< path => Hashtbl.replace(nameForPath, path, name);
  //   | _ => ()
  // };

  // dependencyModules |> List.iter(((modName, paths)) => {
  //   add(modName, paths);
  //   Hashtbl.replace(pathsForModule, modName, paths)
  // });

  // localModules |> List.iter(((modName, paths)) => {
  //   add(modName, paths);
  //   Hashtbl.replace(pathsForModule, modName, paths)
  // });

  (
    pathsForModule,
    nameForPath,
    localModuleNames->hashtblKeys,
    depsModuleNames->hashtblKeys,
    build
  );

};

let getFlags = base =>
  RResult.InfixResult.(
    Files.readFile(base ++ "/.merlin")
    |> RResult.orError("no .merlin file")
    |?>> parseMerlin
    |?>> (((_, _, flags)) => flags |> List.rev)
  );

let getBackend = (rootPath) => {
  let path = Filename.concat(rootPath, ".merlin");
  switch (Files.maybeStat(path)) {
  | Some({Unix.st_kind: Unix.S_REG}) =>
    let ic = open_in(path);
    let rec loop = () =>
      switch (input_line(ic)) {
      | exception End_of_file =>
        close_in(ic);
        Ok("js")
      | s =>
        if (s == "####{BSB GENERATED: NO EDIT") {
          switch(input_line(ic)) {
            | exception End_of_file => Error("Bsb merlin comment not ended correctly");
            | backendLine =>
            let len = String.length("# -backend ");
            let totalLen = String.length(backendLine);
            try (Ok(String.sub(backendLine, len, totalLen - len))) {
              | _ => Ok("native")
            }
          }
        } else {
          loop()
        }
      };
    loop()
  | _ => Ok("js")
  }
}
