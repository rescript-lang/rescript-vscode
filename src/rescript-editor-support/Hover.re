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
    (~name, topLevel: list(SharedTypes.declared(SharedTypes.moduleItem))) => {
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
  Some(codeBlock(full));
};

let showModule =
    (
      ~file: SharedTypes.file,
      ~name,
      declared: option(SharedTypes.declared(SharedTypes.moduleKind)),
    ) => {
  switch (declared) {
  | None => showModuleTopLevel(~name, file.contents.topLevel)
  | Some({item: Structure({topLevel})}) =>
    showModuleTopLevel(~name, topLevel)
  | Some({item: Ident(_)}) => Some("Unable to resolve module reference")
  };
};

let newHover = (~rootUri, ~file: SharedTypes.file, ~getModule, loc) => {
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
    let name =
      switch (declared) {
      | Some(d) => d.name.txt
      | None => file.moduleName
      };
    showModule(~name, ~file, declared);
  | LModule(GlobalReference(moduleName, path, tip)) =>
    let%opt file = getModule(moduleName);
    let env = {Query.file, exported: file.contents.exported};
    let%opt (env, name) = Query.resolvePath(~env, ~path, ~getModule);
    let%opt stamp = Query.exportedForTip(~env, name, tip);
    let%opt md = Hashtbl.find_opt(file.stamps.modules, stamp);
    let%opt (file, declared) =
      References.resolveModuleReference(~file, ~getModule, md);
    let name =
      switch (declared) {
      | Some(d) => d.name.txt
      | None => file.moduleName
      };
    showModule(~name, ~file, declared);
  | LModule(NotFound) => None
  | TopLevelModule(name) =>
    let%opt file = getModule(name);
    showModule(~name=file.moduleName, ~file, None);
  | Typed(_, Definition(_, Attribute(_) | Constructor(_))) => None
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
        let env = {Query.file, exported: file.contents.exported};
        let%opt path = typ |> Shared.digConstructor;
        let%opt (_env, {docstring, name: {txt}, item: {decl}}) =
          digConstructor(~env, ~getModule, path);
        Some((decl |> Shared.declToString(txt), docstring));
      };
      let (typeString, docstring) =
        switch (extraTypeInfo) {
        | None => (typeString, docstring)
        | Some((extra, extraDocstring)) => (
            typeString ++ "\n\n" ++ codeBlock(extra),
            extraDocstring,
          )
        };
      (Some(typeString), docstring);
    };

    let parts =
      switch (References.definedForLoc(~file, ~getModule, locKind)) {
      | None =>
        let (typeString, docstring) = t |> fromType(~docstring=None);
        [typeString, docstring];
      | Some((docstring, {uri}, res)) =>
        let uri =
          Utils.startsWith(uri, rootUri)
            ? "<root>" ++ Utils.sliceToEnd(uri, String.length(rootUri)) : uri;

        let parts =
          switch (res) {
          | `Declared =>
            let (typeString, docstring) = t |> fromType(~docstring);
            [typeString, docstring];
          | `Constructor({cname: {txt}, args}) =>
            let (typeString, docstring) = t |> fromType(~docstring);
            [
              typeString,
              Some(
                codeBlock(
                  txt
                  ++ "("
                  ++ (
                    args
                    |> List.map(((t, _)) => {
                         let typeString = t |> Shared.typeToString;
                         typeString;
                       })
                    |> String.concat(", ")
                  )
                  ++ ")",
                ),
              ),
              docstring,
            ];
          | `Attribute({typ}) =>
            let (typeString, docstring) = typ |> fromType(~docstring);
            [typeString, docstring];
          };

        parts @ [Some(uri)];
      };

    Some(String.concat("\n\n", parts |> Utils.filterMap(x => x)));
  };
};
