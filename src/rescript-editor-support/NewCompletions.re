open SharedTypes;

open Infix;
let showConstructor = ({cname: {txt}, args, res}) => {
  txt
  ++ (
    args == []
      ? ""
      : "("
        ++ String.concat(
             ", ",
             args |> List.map(((typ, _)) => typ |> Shared.typeToString),
           )
        ++ ")"
  )
  ++ (res |?>> (typ => "\n" ++ (typ |> Shared.typeToString)) |? "");
};

/* TODO local opens */
let resolveOpens = (~env, ~previous, opens, ~getModule) =>
  List.fold_left(
    (previous, path) => {
      /** Finding an open, first trying to find it in previoulsly resolved opens */
      let rec loop = prev =>
        switch (prev) {
        | [] =>
          switch (path) {
          | Tip(_) => previous
          | Nested(name, path) =>
            switch (getModule(name)) {
            | None =>
              Log.log("Could not get module " ++ name);
              previous; /* TODO warn? */
            | Some(file) =>
              switch (
                Query.resolvePath(
                  ~env=Query.fileEnv(file),
                  ~getModule,
                  ~path,
                )
              ) {
              | None =>
                Log.log("Could not resolve in " ++ name);
                previous;
              | Some((env, _placeholder)) => previous @ [env]
              }
            }
          }
        | [env, ...rest] =>
          switch (Query.resolvePath(~env, ~getModule, ~path)) {
          | None => loop(rest)
          | Some((env, _placeholder)) => previous @ [env]
          }
        };
      Log.log("resolving open " ++ pathToString(path));
      switch (Query.resolvePath(~env, ~getModule, ~path)) {
      | None =>
        Log.log("Not local");
        loop(previous);
      | Some((env, _)) =>
        Log.log("Was local");
        previous @ [env];
      };
    },
    /* loop(previous) */
    previous,
    opens,
  );

let completionForDeclareds = (~pos, declareds, prefix, transformContents) =>
  /* Log.log("complete for declares " ++ prefix); */
  Hashtbl.fold(
    (_stamp, declared, results) =>
      if (Utils.startsWith(declared.name.txt, prefix)
          && Utils.locationContainsFuzzy(declared.scopeLoc, pos)) {
        [{...declared, item: transformContents(declared.item)}, ...results];
      } else {
        /* Log.log("Nope doesn't count " ++ Utils.showLocation(declared.scopeLoc) ++ " " ++ m); */
        results;
      },
    declareds,
    [],
  );

let completionForExporteds =
    (
      exporteds,
      stamps: Hashtbl.t(int, SharedTypes.declared('a)),
      prefix,
      transformContents,
    ) =>
  Hashtbl.fold(
    (name, stamp, results) =>
      /* Log.log("checking exported: " ++ name); */
      if (Utils.startsWith(name, prefix)) {
        let declared = Hashtbl.find(stamps, stamp);
        [{...declared, item: transformContents(declared.item)}, ...results];
      } else {
        results;
      },
    exporteds,
    [],
  );

let completionForConstructors =
    (
      exportedTypes,
      stamps: Hashtbl.t(int, SharedTypes.declared(SharedTypes.Type.t)),
      prefix,
    ) => {
  Hashtbl.fold(
    (_name, stamp, results) => {
      let t = Hashtbl.find(stamps, stamp);
      switch (t.item.kind) {
      | SharedTypes.Type.Variant(constructors) =>
        {
          constructors
          |> List.filter(c => Utils.startsWith(c.cname.txt, prefix))
          |> List.map(c => (c, t));
        }
        @ results
      | _ => results
      };
    },
    exportedTypes,
    [],
  );
};

let completionForFields =
    (
      exportedTypes,
      stamps: Hashtbl.t(int, SharedTypes.declared(SharedTypes.Type.t)),
      prefix,
    ) =>
  Hashtbl.fold(
    (_name, stamp, results) => {
      let t = Hashtbl.find(stamps, stamp);
      switch (t.item.kind) {
      | Record(fields) =>
        (
          fields
          |> List.filter(f => Utils.startsWith(f.fname.txt, prefix))
          |> List.map(f => (f, t))
        )
        @ results
      | _ => results
      };
    },
    exportedTypes,
    [],
  );

let isCapitalized = name =>
  if (name == "") {
    false;
  } else {
    let c = name.[0];
    switch (c) {
    | 'A'..'Z' => true
    | _ => false
    };
  };

let determineCompletion = items => {
  let rec loop = (offset, items) =>
    switch (items) {
    | [] => assert(false)
    | [one] => `Normal(Tip(one))
    | [one, two] when !isCapitalized(one) => `Attribute(([one], two))
    | [one, two] => `Normal(Nested(one, Tip(two)))
    | [one, ...rest] =>
      if (isCapitalized(one)) {
        switch (loop(offset + String.length(one) + 1, rest)) {
        | `Normal(path) => `Normal(Nested(one, path))
        | x => x
        };
      } else {
        switch (loop(offset + String.length(one) + 1, rest)) {
        | `Normal(path) => `AbsAttribute(path)
        | `Attribute(path, suffix) => `Attribute(([one, ...path], suffix))
        | x => x
        };
      }
    };
  loop(0, items);
};

/* Note: This is a hack. It will be wrong some times if you have a local thing
   that overrides an open.

   Maybe the way to fix it is to make note of what things in an open override
   locally defined things...
   */
let getEnvWithOpens =
    (
      ~pos,
      ~env: Query.queryEnv,
      ~getModule,
      ~opens: list(Query.queryEnv),
      path,
    ) =>
  /* let%opt declared = ; */
  /* for ppx, I think I'd like a "if this is nonnull, bail w/ it".
     So the opposite of let%opt - let%bail or something */
  /* Query.resolvePath(~env, ~path, ~getModule) */
  switch (Query.resolveFromStamps(~env, ~path, ~getModule, ~pos)) {
  | Some(x) => Some(x)
  | None =>
    let rec loop = opens =>
      switch (opens) {
      | [env, ...rest] =>
        Log.log("Looking for env in " ++ Uri2.toString(env.Query.file.uri));
        switch (Query.resolvePath(~env, ~getModule, ~path)) {
        | Some(x) => Some(x)
        | None => loop(rest)
        };
      | [] =>
        switch (path) {
        | Tip(_) => None
        | Nested(top, path) =>
          Log.log("Getting module " ++ top);
          let%opt file = getModule(top);
          Log.log("got it");
          let env = Query.fileEnv(file);
          Query.resolvePath(~env, ~getModule, ~path)
          |> Infix.logIfAbsent("Unable to resolve the path");
        }
      };
    loop(opens);
  };

type k =
  | Module(moduleKind)
  | Value(Types.type_expr)
  | Type(Type.t)
  | Constructor(constructor, declared(Type.t))
  | Field(field, declared(Type.t))
  | FileModule(string);

let kindToInt = k =>
  switch (k) {
  | Module(_) => 9
  | FileModule(_) => 9
  | Constructor(_, _) => 4
  | Field(_, _) => 5
  | Type(_) => 22
  | Value(_) => 12
  };

let detail = (name, contents) =>
  switch (contents) {
  | Type({decl}) => decl |> Shared.declToString(name)
  | Value(typ) => typ |> Shared.typeToString
  | Module(_) => "module"
  | FileModule(_) => "file module"
  | Field({typ}, t) =>
    name
    ++ ": "
    ++ (typ |> Shared.typeToString)
    ++ "\n\n"
    ++ (t.item.decl |> Shared.declToString(t.name.txt))
  | Constructor(c, t) =>
    showConstructor(c)
    ++ "\n\n"
    ++ (t.item.decl |> Shared.declToString(t.name.txt))
  };

let localValueCompletions = (~pos, ~env: Query.queryEnv, suffix) => {
  let results = [];
  Log.log("---------------- LOCAL VAL");
  let results =
    if (suffix == "" || isCapitalized(suffix)) {
      results
      @ completionForDeclareds(~pos, env.file.stamps.modules, suffix, m =>
          Module(m)
        )
      @ (
        /* TODO declared thingsz */
        completionForConstructors(
          env.exported.types,
          env.file.stamps.types,
          suffix,
        )
        |> List.map(((c, t)) =>
             {...emptyDeclared(c.cname.txt), item: Constructor(c, t)}
           )
      );
    } else {
      results;
    };

  let results =
    if (suffix == "" || !isCapitalized(suffix)) {
      results
      @ completionForDeclareds(~pos, env.file.stamps.values, suffix, v =>
          Value(v)
        )
      @ completionForDeclareds(~pos, env.file.stamps.types, suffix, t =>
          Type(t)
        )
      @ (
        completionForFields(env.exported.types, env.file.stamps.types, suffix)
        |> List.map(((f, t)) =>
             {...emptyDeclared(f.fname.txt), item: Field(f, t)}
           )
      );
    } else {
      results;
    };

  results |> List.map(x => (env.file.uri, x));
};

let valueCompletions = (~env: Query.queryEnv, suffix) => {
  Log.log(" - Completing in " ++ Uri2.toString(env.file.uri));
  let results = [];
  let results =
    if (suffix == "" || isCapitalized(suffix)) {
      // Get rid of lowercase modules (#417)
      env.exported.modules
      |> Hashtbl.filter_map_inplace((name, key) =>
           isCapitalized(name) ? Some(key) : None
         );

      let moduleCompletions =
        completionForExporteds(
          env.exported.modules, env.file.stamps.modules, suffix, m =>
          Module(m)
        );
      /* Log.log(" -- capitalized " ++ string_of_int(Hashtbl.length(env.exported.types)) ++ " exported types"); */
      /* env.exported.types |> Hashtbl.iter((name, _) => Log.log("    > " ++ name)); */
      results
      @ moduleCompletions
      @ (
        /* TODO declared thingsz */
        completionForConstructors(
          env.exported.types,
          env.file.stamps.types,
          suffix,
        )
        |> List.map(((c, t)) =>
             {...emptyDeclared(c.cname.txt), item: Constructor(c, t)}
           )
      );
    } else {
      results;
    };

  let results =
    if (suffix == "" || !isCapitalized(suffix)) {
      Log.log(" -- not capitalized");
      results
      @ completionForExporteds(
          env.exported.values, env.file.stamps.values, suffix, v =>
          Value(v)
        )
      @ completionForExporteds(
          env.exported.types, env.file.stamps.types, suffix, t =>
          Type(t)
        )
      @ (
        completionForFields(env.exported.types, env.file.stamps.types, suffix)
        |> List.map(((f, t)) =>
             {...emptyDeclared(f.fname.txt), item: Field(f, t)}
           )
      );
    } else {
      results;
    };

  /* Log.log("Getting value completions " ++ env.file.uri);
     Log.log(String.concat(", ", results |. Belt.List.map(x => x.name.txt))); */

  results |> List.map(x => (env.file.uri, x));
};

let attributeCompletions = (~env: Query.queryEnv, ~suffix) => {
  let results = [];
  let results =
    if (suffix == "" || isCapitalized(suffix)) {
      results
      @ completionForExporteds(
          env.exported.modules, env.file.stamps.modules, suffix, m =>
          Module(m)
        );
    } else {
      results;
    };

  let results =
    if (suffix == "" || !isCapitalized(suffix)) {
      results
      @ completionForExporteds(
          env.exported.values, env.file.stamps.values, suffix, v =>
          Value(v)
        )
      /* completionForExporteds(env.exported.types, env.file.stamps.types, suffix, t => Type(t)) @ */
      @ (
        completionForFields(env.exported.types, env.file.stamps.types, suffix)
        |> List.map(((f, t)) =>
             {...emptyDeclared(f.fname.txt), item: Field(f, t)}
           )
      );
    } else {
      results;
    };

  results |> List.map(x => (env.file.uri, x));
};

/**

TODO filter out things that are defined after the current position

*/

let resolveRawOpens = (~env, ~getModule, ~rawOpens, ~package) => {
  // TODO Stdlib instead of Pervasives
  let packageOpens = ["Pervasives", ...package.TopTypes.opens];
  Log.log("Package opens " ++ String.concat(" ", packageOpens));

  let opens =
    resolveOpens(
      ~env,
      ~previous=
        List.map(Query.fileEnv, packageOpens |> Utils.filterMap(getModule)),
      rawOpens,
      ~getModule,
    );

  opens;
};

let getItems =
    (~full, ~package, ~rawOpens, ~getModule, ~allModules, ~pos, ~parts) => {
  Log.log(
    "Opens folkz > "
    ++ string_of_int(List.length(rawOpens))
    ++ " "
    ++ String.concat(" ... ", rawOpens |> List.map(pathToString)),
  );
  let env = Query.fileEnv(full.file);

  let packageOpens = ["Pervasives", ...package.TopTypes.opens];
  Log.log("Package opens " ++ String.concat(" ", packageOpens));

  let opens = resolveRawOpens(~env, ~getModule, ~rawOpens, ~package);
  Log.log(
    "Opens nows "
    ++ string_of_int(List.length(opens))
    ++ " "
    ++ String.concat(
         " ",
         opens |> List.map(e => Uri2.toString(e.Query.file.uri)),
       ),
  );

  switch (parts) {
  | [] => []
  | [suffix] =>
    let locallyDefinedValues = localValueCompletions(~pos, ~env, suffix);
    let alreadyUsedIdentifiers = Hashtbl.create(10);
    let valuesFromOpens =
      opens
      |> List.fold_left(
           (results, env) => {
             let completionsFromThisOpen = valueCompletions(~env, suffix);
             List.filter(
               ((_uri, declared)) =>
                 if (!Hashtbl.mem(alreadyUsedIdentifiers, declared.name.txt)) {
                   Hashtbl.add(
                     alreadyUsedIdentifiers,
                     declared.name.txt,
                     true,
                   );
                   true;
                 } else {
                   false;
                 },
               completionsFromThisOpen,
             )
             @ results;
           },
           [],
         );
    /* TODO complete the namespaced name too */
    let localModuleNames =
      allModules
      |> Utils.filterMap(name => {
           /* Log.log("Checking " ++ name); */
           Utils.startsWith(name, suffix) && !String.contains(name, '-')
             ? Some((
                 env.file.uri,
                 {...emptyDeclared(name), item: FileModule(name)},
               ))
             : None
         });
    locallyDefinedValues @ valuesFromOpens @ localModuleNames;
  | multiple =>
    open Infix;
    Log.log("Completing for " ++ String.concat("<.>", multiple));

    switch (determineCompletion(multiple)) {
    | `Normal(path) =>
      Log.log("normal " ++ pathToString(path));
      switch (getEnvWithOpens(~pos, ~env, ~getModule, ~opens, path)) {
      | Some((env, suffix)) =>
        Log.log("Got the env");
        valueCompletions(~env, suffix);
      | None => []
      };
    | `Attribute(target, suffix) =>
      {
        Log.log("suffix :" ++ suffix);
        switch (target) {
        | [] => None
        | [first, ...rest] =>
          Log.log("-------------- Looking for " ++ first);
          let%opt declared =
            Query.findInScope(pos, first, env.file.stamps.values);
          Log.log("Found it! " ++ declared.name.txt);
          let%opt path = declared.item |> Shared.digConstructor;
          let%opt (env, typ) = Hover.digConstructor(~env, ~getModule, path);
          let%opt (env, typ) =
            rest
            |> List.fold_left(
                 (current, name) => {
                   let%opt (env, typ) = current;
                   switch (typ.item.SharedTypes.Type.kind) {
                   | Record(fields) =>
                     let%opt attr =
                       fields |> List.find_opt(f => f.fname.txt == name);
                     Log.log("Found attr " ++ name);
                     let%opt path = attr.typ |> Shared.digConstructor;
                     Hover.digConstructor(~env, ~getModule, path);
                   | _ => None
                   };
                 },
                 Some((env, typ)),
               );
          switch (typ.item.kind) {
          | Record(fields) =>
            Some(
              fields
              |> Utils.filterMap(f =>
                   if (Utils.startsWith(f.fname.txt, suffix)) {
                     Some((
                       env.file.uri,
                       {...emptyDeclared(f.fname.txt), item: Field(f, typ)},
                     ));
                   } else {
                     None;
                   }
                 ),
            )
          | _ => None
          };
        };
      }
      |? []
    | `AbsAttribute(path) =>
      switch (getEnvWithOpens(~pos, ~env, ~getModule, ~opens, path)) {
      | None => []
      | Some((env, suffix)) =>
        attributeCompletions(~env, ~suffix)
        @ List.concat(
            opens |> List.map(env => attributeCompletions(~env, ~suffix)),
          )
      }
    };
  };
};

module J = JsonShort;

let computeCompletions = (~full, ~maybeText, ~package, ~pos, ~state) => {
  let parameters =
    switch (maybeText) {
    | None => None
    | Some(text) =>
      switch (PartialParser.positionToOffset(text, pos)) {
      | None => None
      | Some(offset) =>
        switch (PartialParser.findCompletable(text, offset)) {
        | None => None
        | Some(Clabel(_)) =>
          /* Not supported yet */
          None
        | Some(Cpath(parts)) => Some((text, offset, parts))
        }
      }
    };
  let items =
    switch (parameters) {
    | None => []
    | Some((text, offset, parts)) =>
      let rawOpens = PartialParser.findOpens(text, offset);
      let allModules =
        package.TopTypes.localModules @ package.dependencyModules;
      let items =
        getItems(
          ~full,
          ~package,
          ~rawOpens,
          ~getModule=State.fileForModule(state, ~package),
          ~allModules,
          ~pos,
          ~parts,
        );
      /* TODO(#107): figure out why we're getting duplicates. */
      Utils.dedup(items);
    };
  if (items == []) {
    J.null;
  } else {
    items
    |> List.map(
         (
           (
             uri,
             {
               SharedTypes.name: {txt: name, loc: {loc_start: {pos_lnum}}},
               docstring,
               item,
             },
           ),
         ) => {
         J.o([
           ("label", J.s(name)),
           ("kind", J.i(kindToInt(item))),
           ("detail", detail(name, item) |> J.s),
           (
             "documentation",
             J.o([
               ("kind", J.s("markdown")),
               (
                 "value",
                 J.s(
                   (docstring |? "No docs")
                   ++ "\n\n"
                   ++ Uri2.toString(uri)
                   ++ ":"
                   ++ string_of_int(pos_lnum),
                 ),
               ),
             ]),
           ),
         ])
       })
    |> J.l;
  };
};
