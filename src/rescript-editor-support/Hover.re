let digConstructor = (~env, ~getModule, path) => {
  switch (Query.resolveFromCompilerPath(~env, ~getModule, path)) {
  | `Not_found => None
  | `Stamp(stamp) =>
    let%opt t = Hashtbl.find_opt(env.file.stamps.types, stamp);
    Some((env, t));
  | `Exported(env, name) =>
    let%opt stamp = Hashtbl.find_opt(env.exported.types, name);
    let%opt t = Hashtbl.find_opt(env.file.stamps.types, stamp);
    Some((env, t));
  | _ => None
  };
};

let codeBlock = code => {
  Printf.sprintf("```rescript\n%s\n```", code);
};

let showModuleTopLevel =
    (
      ~docstring,
      ~name,
      topLevel: list(SharedTypes.declared(SharedTypes.moduleItem)),
    ) => {
  let contents =
    topLevel
    |> List.map(item =>
         switch (item.SharedTypes.item) {
         /*** TODO pretty print module contents */
         | SharedTypes.MType({decl}, recStatus) =>
           "  " ++ (decl |> Shared.declToString(~recStatus, item.name.txt))
         | Module(_) => "  module " ++ item.name.txt
         | MValue(typ) =>
           "  let " ++ item.name.txt ++ ": " ++ (typ |> Shared.typeToString) /* TODO indent */
         }
       )
    |> String.concat("\n");
  let full = "module " ++ name ++ " = {" ++ "\n" ++ contents ++ "\n}";
  let doc =
    switch (docstring) {
    | [] => ""
    | [_, ..._] => "\n" ++ (docstring |> String.concat("\n")) ++ "\n"
    };
  Some(doc ++ codeBlock(full));
};

let showModule =
    (
      ~docstring,
      ~file: SharedTypes.file,
      ~name,
      declared: option(SharedTypes.declared(SharedTypes.moduleKind)),
    ) => {
  switch (declared) {
  | None => showModuleTopLevel(~docstring, ~name, file.contents.topLevel)
  | Some({item: Structure({topLevel})}) =>
    showModuleTopLevel(~docstring, ~name, topLevel)
  | Some({item: Ident(_)}) => Some("Unable to resolve module reference")
  };
};

let newHover = (~file: SharedTypes.file, ~getModule, loc) => {
  switch (loc) {
  | SharedTypes.Explanation(text) => Some(text)
  | TypeDefinition(name, decl, _stamp) =>
    let typeDef = Shared.declToString(name, decl);
    Some(codeBlock(typeDef));
  | LModule(Definition(stamp, _tip))
  | LModule(LocalReference(stamp, _tip)) =>
    let%opt md = Hashtbl.find_opt(file.stamps.modules, stamp);
    let%opt (file, declared) =
      References.resolveModuleReference(~file, ~getModule, md);
    let (name, docstring) =
      switch (declared) {
      | Some(d) => (d.name.txt, d.docstring)
      | None => (file.moduleName, file.contents.docstring)
      };
    showModule(~docstring, ~name, ~file, declared);
  | LModule(GlobalReference(moduleName, path, tip)) =>
    let%opt file = getModule(moduleName);
    let env = Query.fileEnv(file);
    let%opt (env, name) = Query.resolvePath(~env, ~path, ~getModule);
    let%opt stamp = Query.exportedForTip(~env, name, tip);
    let%opt md = Hashtbl.find_opt(file.stamps.modules, stamp);
    let%opt (file, declared) =
      References.resolveModuleReference(~file, ~getModule, md);
    let (name, docstring) =
      switch (declared) {
      | Some(d) => (d.name.txt, d.docstring)
      | None => (file.moduleName, file.contents.docstring)
      };
    showModule(~docstring, ~name, ~file, declared);
  | LModule(NotFound) => None
  | TopLevelModule(name) =>
    let%opt file = getModule(name);
    showModule(
      ~docstring=file.contents.docstring,
      ~name=file.moduleName,
      ~file,
      None,
    );
  | Typed(_, Definition(_, Field(_) | Constructor(_))) => None
  | Constant(t) =>
    Some(
      switch (t) {
      | Const_int(_) => "int"
      | Const_char(_) => "char"
      | Const_string(_) => "string"
      | Const_float(_) => "float"
      | Const_int32(_) => "int32"
      | Const_int64(_) => "int64"
      | Const_nativeint(_) => "int"
      },
    )
  | Typed(t, locKind) =>
    let fromType = (~docstring, typ) => {
      let typeString = codeBlock(typ |> Shared.typeToString);
      let extraTypeInfo = {
        let env = Query.fileEnv(file);
        let%opt path = typ |> Shared.digConstructor;
        let%opt (_env, {docstring, name: {txt}, item: {decl}}) =
          digConstructor(~env, ~getModule, path);
        let isUncurriedInternal =
          Utils.startsWith(Path.name(path), "Js.Fn.arity");
        if (isUncurriedInternal) {
          None;
        } else {
          Some((decl |> Shared.declToString(txt), docstring));
        };
      };
      let (typeString, docstring) =
        switch (extraTypeInfo) {
        | None => (typeString, docstring)
        | Some((extra, extraDocstring)) => (
            typeString ++ "\n\n" ++ codeBlock(extra),
            extraDocstring,
          )
        };
      (typeString, docstring);
    };

    let parts =
      switch (References.definedForLoc(~file, ~getModule, locKind)) {
      | None =>
        let (typeString, docstring) = t |> fromType(~docstring=[]);
        [typeString, ...docstring];
      | Some((docstring, res)) =>
        switch (res) {
        | `Declared =>
          let (typeString, docstring) = t |> fromType(~docstring);
          [typeString, ...docstring];
        | `Constructor({cname: {txt}, args}) =>
          let (typeString, docstring) = t |> fromType(~docstring);

          let argsString =
            switch (args) {
            | [] => ""
            | _ =>
              args
              |> List.map(((t, _)) => Shared.typeToString(t))
              |> String.concat(", ")
              |> Printf.sprintf("(%s)")
            };

          [typeString, codeBlock(txt ++ argsString), ...docstring];
        | `Field({typ}) =>
          let (typeString, docstring) = typ |> fromType(~docstring);
          [typeString, ...docstring];
        }
      };

    Some(String.concat("\n\n", parts));
  };
};
