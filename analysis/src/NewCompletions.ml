open SharedTypes

let showConstructor {cname = {txt}; args; res} =
  txt
  ^ (match args with
    | [] -> ""
    | _ ->
      "("
      ^ (args
        |> List.map (fun (typ, _) -> typ |> Shared.typeToString)
        |> String.concat ", ")
      ^ ")")
  ^ match res with None -> "" | Some typ -> "\n" ^ (typ |> Shared.typeToString)

(* TODO: local opens *)
let resolveOpens ~env ~previous opens ~package =
  List.fold_left
    (fun previous path ->
      (* Finding an open, first trying to find it in previoulsly resolved opens *)
      let rec loop prev =
        match prev with
        | [] -> (
          match path with
          | Tip _ -> previous
          | Nested (name, path) -> (
            match ProcessCmt.fileForModule ~package name with
            | None ->
              Log.log ("Could not get module " ^ name);
              previous (* TODO: warn? *)
            | Some file -> (
              match
                ProcessCmt.resolvePath ~env:(QueryEnv.fromFile file) ~package
                  ~path
              with
              | None ->
                Log.log ("Could not resolve in " ^ name);
                previous
              | Some (env, _placeholder) -> previous @ [env])))
        | env :: rest -> (
          match ProcessCmt.resolvePath ~env ~package ~path with
          | None -> loop rest
          | Some (env, _placeholder) -> previous @ [env])
      in
      Log.log ("resolving open " ^ pathToString path);
      match ProcessCmt.resolvePath ~env ~package ~path with
      | None ->
        Log.log "Not local";
        loop previous
      | Some (env, _) ->
        Log.log "Was local";
        previous @ [env])
    (* loop(previous) *)
    previous opens

let completionForDeclareds ~pos declareds prefix transformContents =
  (* Log.log("completion for declares " ++ prefix); *)
  Hashtbl.fold
    (fun _stamp declared results ->
      if
        Utils.startsWith declared.name.txt prefix
        && Utils.locationContainsFuzzy declared.scopeLoc pos
      then {declared with item = transformContents declared.item} :: results
      else
        (* Log.log("Nope doesn't count " ++ Utils.showLocation(declared.scopeLoc) ++ " " ++ m); *)
        results)
    declareds []

let completionForExporteds exporteds
    (stamps : (int, 'a SharedTypes.declared) Hashtbl.t) prefix transformContents
    =
  Hashtbl.fold
    (fun name stamp results ->
      (* Log.log("checking exported: " ++ name); *)
      if Utils.startsWith name prefix then
        let declared = Hashtbl.find stamps stamp in
        {declared with item = transformContents declared.item} :: results
      else results)
    exporteds []

let completionForConstructors exportedTypes
    (stamps : (int, SharedTypes.Type.t SharedTypes.declared) Hashtbl.t) prefix =
  Hashtbl.fold
    (fun _name stamp results ->
      let t = Hashtbl.find stamps stamp in
      match t.item.kind with
      | SharedTypes.Type.Variant constructors ->
        (constructors
        |> List.filter (fun c -> Utils.startsWith c.cname.txt prefix)
        |> List.map (fun c -> (c, t)))
        @ results
      | _ -> results)
    exportedTypes []

let completionForFields exportedTypes
    (stamps : (int, SharedTypes.Type.t SharedTypes.declared) Hashtbl.t) prefix =
  Hashtbl.fold
    (fun _name stamp results ->
      let t = Hashtbl.find stamps stamp in
      match t.item.kind with
      | Record fields ->
        (fields
        |> List.filter (fun f -> Utils.startsWith f.fname.txt prefix)
        |> List.map (fun f -> (f, t)))
        @ results
      | _ -> results)
    exportedTypes []

let isCapitalized name =
  if name = "" then false
  else
    let c = name.[0] in
    match c with 'A' .. 'Z' -> true | _ -> false

let determineCompletion items =
  let rec loop offset items =
    match items with
    | [] -> assert false
    | [one] -> `Normal (Tip one)
    | [one; two] when not (isCapitalized one) -> `Attribute ([one], two)
    | [one; two] -> `Normal (Nested (one, Tip two))
    | one :: rest -> (
      if isCapitalized one then
        match loop (offset + String.length one + 1) rest with
        | `Normal path -> `Normal (Nested (one, path))
        | x -> x
      else
        match loop (offset + String.length one + 1) rest with
        | `Normal path -> `AbsAttribute path
        | `Attribute (path, suffix) -> `Attribute (one :: path, suffix)
        | x -> x)
  in
  loop 0 items

(* Note: This is a hack. It will be wrong some times if you have a local thing
   that overrides an open.

   Maybe the way to fix it is to make note of what things in an open override
   locally defined things...
*)
let getEnvWithOpens ~pos ~(env : QueryEnv.t) ~package ~(opens : QueryEnv.t list)
    path =
  match ProcessCmt.resolveFromStamps ~env ~path ~package ~pos with
  | Some x -> Some x
  | None ->
    let rec loop opens =
      match opens with
      | (env : QueryEnv.t) :: rest -> (
        Log.log ("Looking for env in " ^ Uri2.toString env.file.uri);
        match ProcessCmt.resolvePath ~env ~package ~path with
        | Some x -> Some x
        | None -> loop rest)
      | [] -> (
        match path with
        | Tip _ -> None
        | Nested (top, path) -> (
          Log.log ("Getting module " ^ top);
          match ProcessCmt.fileForModule ~package top with
          | None -> None
          | Some file ->
            Log.log "got it";
            let env = QueryEnv.fromFile file in
            ProcessCmt.resolvePath ~env ~package ~path))
    in
    loop opens

type k =
  | Module of moduleKind
  | Value of Types.type_expr
  | Type of Type.t
  | Constructor of constructor * Type.t declared
  | Field of field * Type.t declared
  | FileModule of string

let kindToInt k =
  match k with
  | Module _ -> 9
  | FileModule _ -> 9
  | Constructor (_, _) -> 4
  | Field (_, _) -> 5
  | Type _ -> 22
  | Value _ -> 12

let detail name contents =
  match contents with
  | Type {decl} -> decl |> Shared.declToString name
  | Value typ -> typ |> Shared.typeToString
  | Module _ -> "module"
  | FileModule _ -> "file module"
  | Field ({typ}, t) ->
    name ^ ": "
    ^ (typ |> Shared.typeToString)
    ^ "\n\n"
    ^ (t.item.decl |> Shared.declToString t.name.txt)
  | Constructor (c, t) ->
    showConstructor c ^ "\n\n" ^ (t.item.decl |> Shared.declToString t.name.txt)

let localValueCompletions ~pos ~(env : QueryEnv.t) suffix =
  let results = [] in
  Log.log "---------------- LOCAL VAL";
  let results =
    if suffix = "" || isCapitalized suffix then
      results
      @ completionForDeclareds ~pos env.file.stamps.modules suffix (fun m ->
            Module m)
      @ (completionForConstructors env.exported.types env.file.stamps.types
           (* TODO declared thingsz *)
           suffix
        |> List.map (fun (c, t) ->
               {(emptyDeclared c.cname.txt) with item = Constructor (c, t)}))
    else results
  in
  if suffix = "" || not (isCapitalized suffix) then
    results
    @ completionForDeclareds ~pos env.file.stamps.values suffix (fun v ->
          Value v)
    @ completionForDeclareds ~pos env.file.stamps.types suffix (fun t -> Type t)
    @ (completionForFields env.exported.types env.file.stamps.types suffix
      |> List.map (fun (f, t) ->
             {(emptyDeclared f.fname.txt) with item = Field (f, t)}))
  else results

let valueCompletions ~(env : QueryEnv.t) suffix =
  Log.log (" - Completing in " ^ Uri2.toString env.file.uri);
  let results = [] in
  let results =
    if suffix = "" || isCapitalized suffix then (
      (* Get rid of lowercase modules (#417) *)
      env.exported.modules
      |> Hashtbl.filter_map_inplace (fun name key ->
             if isCapitalized name then Some key else None);
      let moduleCompletions =
        completionForExporteds env.exported.modules env.file.stamps.modules
          suffix (fun m -> Module m)
      in
      (* Log.log(" -- capitalized " ++ string_of_int(Hashtbl.length(env.exported.types)) ++ " exported types"); *)
      (* env.exported.types |> Hashtbl.iter((name, _) => Log.log("    > " ++ name)); *)
      results @ moduleCompletions
      @ ((* TODO declared thingsz *)
         completionForConstructors env.exported.types env.file.stamps.types
           suffix
        |> List.map (fun (c, t) ->
               {(emptyDeclared c.cname.txt) with item = Constructor (c, t)})))
    else results
  in
  if suffix = "" || not (isCapitalized suffix) then (
    Log.log " -- not capitalized";
    results
    @ completionForExporteds env.exported.values env.file.stamps.values suffix
        (fun v -> Value v)
    @ completionForExporteds env.exported.types env.file.stamps.types suffix
        (fun t -> Type t)
    @ (completionForFields env.exported.types env.file.stamps.types suffix
      |> List.map (fun (f, t) ->
             {(emptyDeclared f.fname.txt) with item = Field (f, t)})))
  else results

let attributeCompletions ~(env : QueryEnv.t) ~suffix =
  let results = [] in
  let results =
    if suffix = "" || isCapitalized suffix then
      results
      @ completionForExporteds env.exported.modules env.file.stamps.modules
          suffix (fun m -> Module m)
    else results
  in
  if suffix = "" || not (isCapitalized suffix) then
    results
    @ completionForExporteds env.exported.values env.file.stamps.values suffix
        (fun v -> Value v)
    (* completionForExporteds(env.exported.types, env.file.stamps.types, suffix, t => Type(t)) @ *)
    @ (completionForFields env.exported.types env.file.stamps.types suffix
      |> List.map (fun (f, t) ->
             {(emptyDeclared f.fname.txt) with item = Field (f, t)}))
  else results

(* TODO filter out things that are defined after the current position *)
let resolveRawOpens ~env ~rawOpens ~package =
  (* TODO Stdlib instead of Pervasives *)
  let packageOpens = "Pervasives" :: package.opens in
  Log.log ("Package opens " ^ String.concat " " packageOpens);
  let opens =
    resolveOpens ~env
      ~previous:
        (List.map QueryEnv.fromFile
           (packageOpens |> Utils.filterMap (ProcessCmt.fileForModule ~package)))
      rawOpens ~package
  in
  opens

let getItems ~full ~package ~rawOpens ~allModules ~pos ~parts =
  Log.log
    ("Opens folkz > "
    ^ string_of_int (List.length rawOpens)
    ^ " "
    ^ String.concat " ... " (rawOpens |> List.map pathToString));
  let env = QueryEnv.fromFile full.file in
  let packageOpens = "Pervasives" :: package.opens in
  Log.log ("Package opens " ^ String.concat " " packageOpens);
  let resolvedOpens = resolveRawOpens ~env ~rawOpens ~package in
  Log.log
    ("Opens nows "
    ^ string_of_int (List.length resolvedOpens)
    ^ " "
    ^ String.concat " "
        (resolvedOpens
        |> List.map (fun (e : QueryEnv.t) -> Uri2.toString e.file.uri)));
  (* Last open takes priority *)
  let opens = List.rev resolvedOpens in
  match parts with
  | [] -> []
  | [suffix] ->
    let locallyDefinedValues = localValueCompletions ~pos ~env suffix in
    let alreadyUsedIdentifiers = Hashtbl.create 10 in
    let valuesFromOpens =
      opens
      |> List.fold_left
           (fun results env ->
             let completionsFromThisOpen = valueCompletions ~env suffix in
             List.filter
               (fun declared ->
                 if Hashtbl.mem alreadyUsedIdentifiers declared.name.txt then
                   false
                 else (
                   Hashtbl.add alreadyUsedIdentifiers declared.name.txt true;
                   true))
               completionsFromThisOpen
             @ results)
           []
    in
    (* TODO complete the namespaced name too *)
    let localModuleNames =
      allModules
      |> Utils.filterMap (fun name ->
             if Utils.startsWith name suffix && not (String.contains name '-')
             then Some {(emptyDeclared name) with item = FileModule name}
             else None)
    in
    locallyDefinedValues @ valuesFromOpens @ localModuleNames
  | multiple -> (
    Log.log ("Completing for " ^ String.concat "<.>" multiple);
    match determineCompletion multiple with
    | `Normal path -> (
      Log.log ("normal " ^ pathToString path);
      match getEnvWithOpens ~pos ~env ~package ~opens path with
      | Some (env, suffix) ->
        Log.log "Got the env";
        valueCompletions ~env suffix
      | None -> [])
    | `Attribute (target, suffix) -> (
      Log.log ("suffix :" ^ suffix);
      match target with
      | [] -> []
      | first :: rest -> (
        Log.log ("-------------- Looking for " ^ first);
        match ProcessCmt.findInScope pos first env.file.stamps.values with
        | None -> []
        | Some declared -> (
          Log.log ("Found it! " ^ declared.name.txt);
          match declared.item |> Shared.digConstructor with
          | None -> []
          | Some path -> (
            match Hover.digConstructor ~env ~package path with
            | None -> []
            | Some (env, typ) -> (
              match
                rest
                |> List.fold_left
                     (fun current name ->
                       match current with
                       | None -> None
                       | Some (env, typ) -> (
                         match typ.item.SharedTypes.Type.kind with
                         | Record fields -> (
                           match
                             fields
                             |> List.find_opt (fun f -> f.fname.txt = name)
                           with
                           | None -> None
                           | Some attr -> (
                             Log.log ("Found attr " ^ name);
                             match attr.typ |> Shared.digConstructor with
                             | None -> None
                             | Some path ->
                               Hover.digConstructor ~env ~package path))
                         | _ -> None))
                     (Some (env, typ))
              with
              | None -> []
              | Some (_env, typ) -> (
                match typ.item.kind with
                | Record fields ->
                  fields
                  |> Utils.filterMap (fun f ->
                         if Utils.startsWith f.fname.txt suffix then
                           Some
                             {
                               (emptyDeclared f.fname.txt) with
                               item = Field (f, typ);
                             }
                         else None)
                | _ -> []))))))
    | `AbsAttribute path -> (
      match getEnvWithOpens ~pos ~env ~package ~opens path with
      | None -> []
      | Some (env, suffix) ->
        attributeCompletions ~env ~suffix
        @ List.concat
            (opens |> List.map (fun env -> attributeCompletions ~env ~suffix))))

let mkItem ~name ~kind ~detail ~deprecated ~docstring =
  let docContent =
    (match deprecated with None -> "" | Some s -> "Deprecated: " ^ s ^ "\n\n")
    ^
    match docstring with [] -> "" | _ :: _ -> docstring |> String.concat "\n"
  in
  let tags =
    match deprecated with None -> [] | Some _ -> [1 (* deprecated *)]
  in
  Protocol.
    {
      label = name;
      kind;
      tags;
      detail;
      documentation =
        (if docContent = "" then None
        else Some {kind = "markdown"; value = docContent});
    }

let processCompletable ~findItems ~package ~rawOpens
    (completable : PartialParser.completable) =
  match completable with
  | Cjsx ([id], prefix, identsSeen) when String.lowercase_ascii id = id ->
    let labels = [("aria-details", "string"); ("aria-disabled", "bool")] in
    (*
    @optional @as("aria-details")
    ariaDetails: string,
    @optional @as("aria-disabled")
    ariaDisabled: bool,
    @optional @as("aria-hidden")
    ariaHidden: bool,
    /* [@optional] [@as "aria-invalid"] ariaInvalid: grammar|false|spelling|true, */
    @optional @as("aria-keyshortcuts")
    ariaKeyshortcuts: string,
    @optional @as("aria-label")
    ariaLabel: string,
    @optional @as("aria-roledescription")
    ariaRoledescription: string,
    /* Widget Attributes */
    /* [@optional] [@as "aria-autocomplete"] ariaAutocomplete: inline|list|both|none, */
    /* [@optional] [@as "aria-checked"] ariaChecked: true|false|mixed, /* https://www.w3.org/TR/wai-aria-1.1/#valuetype_tristate */ */
    @optional @as("aria-expanded")
    ariaExpanded: bool,
    /* [@optional] [@as "aria-haspopup"] ariaHaspopup: false|true|menu|listbox|tree|grid|dialog, */
    @optional @as("aria-level")
    ariaLevel: int,
    @optional @as("aria-modal")
    ariaModal: bool,
    @optional @as("aria-multiline")
    ariaMultiline: bool,
    @optional @as("aria-multiselectable")
    ariaMultiselectable: bool,
    /* [@optional] [@as "aria-orientation"] ariaOrientation: horizontal|vertical|undefined, */
    @optional @as("aria-placeholder")
    ariaPlaceholder: string,
    /* [@optional] [@as "aria-pressed"] ariaPressed: true|false|mixed, /* https://www.w3.org/TR/wai-aria-1.1/#valuetype_tristate */ */
    @optional @as("aria-readonly")
    ariaReadonly: bool,
    @optional @as("aria-required")
    ariaRequired: bool,
    @optional @as("aria-selected")
    ariaSelected: bool,
    @optional @as("aria-sort")
    ariaSort: string,
    @optional @as("aria-valuemax")
    ariaValuemax: float,
    @optional @as("aria-valuemin")
    ariaValuemin: float,
    @optional @as("aria-valuenow")
    ariaValuenow: float,
    @optional @as("aria-valuetext")
    ariaValuetext: string,
    /* Live Region Attributes */
    @optional @as("aria-atomic")
    ariaAtomic: bool,
    @optional @as("aria-busy")
    ariaBusy: bool,
    /* [@optional] [@as "aria-live"] ariaLive: off|polite|assertive|rude, */
    @optional @as("aria-relevant")
    ariaRelevant: string,
    /* Drag-and-Drop Attributes */
    /* [@optional] [@as "aria-dropeffect"] ariaDropeffect: copy|move|link|execute|popup|none, */
    @optional @as("aria-grabbed")
    ariaGrabbed: bool,
    /* Relationship Attributes */
    @optional @as("aria-activedescendant")
    ariaActivedescendant: string,
    @optional @as("aria-colcount")
    ariaColcount: int,
    @optional @as("aria-colindex")
    ariaColindex: int,
    @optional @as("aria-colspan")
    ariaColspan: int,
    @optional @as("aria-controls")
    ariaControls: string,
    @optional @as("aria-describedby")
    ariaDescribedby: string,
    @optional @as("aria-errormessage")
    ariaErrormessage: string,
    @optional @as("aria-flowto")
    ariaFlowto: string,
    @optional @as("aria-labelledby")
    ariaLabelledby: string,
    @optional @as("aria-owns")
    ariaOwns: string,
    @optional @as("aria-posinset")
    ariaPosinset: int,
    @optional @as("aria-rowcount")
    ariaRowcount: int,
    @optional @as("aria-rowindex")
    ariaRowindex: int,
    @optional @as("aria-rowspan")
    ariaRowspan: int,
    @optional @as("aria-setsize")
    ariaSetsize: int,
    /* react textarea/input */
    @optional
    defaultChecked: bool,
    @optional
    defaultValue: string,
    /* global html attributes */
    @optional
    accessKey: string,
    @optional
    className: string /* substitute for "class" */,
    @optional
    contentEditable: bool,
    @optional
    contextMenu: string,
    @optional
    dir: string /* "ltr", "rtl" or "auto" */,
    @optional
    draggable: bool,
    @optional
    hidden: bool,
    @optional
    id: string,
    @optional
    lang: string,
    @optional
    role: string /* ARIA role */,
    @optional
    style: style,
    @optional
    spellCheck: bool,
    @optional
    tabIndex: int,
    @optional
    title: string,
    /* html5 microdata */
    @optional
    itemID: string,
    @optional
    itemProp: string,
    @optional
    itemRef: string,
    @optional
    itemScope: bool,
    @optional
    itemType: string /* uri */,
    /* tag-specific html attributes */
    @optional
    accept: string,
    @optional
    acceptCharset: string,
    @optional
    action: string /* uri */,
    @optional
    allowFullScreen: bool,
    @optional
    alt: string,
    @optional
    async: bool,
    @optional
    autoComplete: string /* has a fixed, but large-ish, set of possible values */,
    @optional
    autoCapitalize: string /* Mobile Safari specific */,
    @optional
    autoFocus: bool,
    @optional
    autoPlay: bool,
    @optional
    challenge: string,
    @optional
    charSet: string,
    @optional
    checked: bool,
    @optional
    cite: string /* uri */,
    @optional
    crossOrigin: string /* anonymous, use-credentials */,
    @optional
    cols: int,
    @optional
    colSpan: int,
    @optional
    content: string,
    @optional
    controls: bool,
    @optional
    coords: string /* set of values specifying the coordinates of a region */,
    @optional
    data: string /* uri */,
    @optional
    dateTime: string /* "valid date string with optional time" */,
    @optional
    default: bool,
    @optional
    defer: bool,
    @optional
    disabled: bool,
    @optional
    download: string /* should really be either a boolean, signifying presence, or a string */,
    @optional
    encType: string /* "application/x-www-form-urlencoded", "multipart/form-data" or "text/plain" */,
    @optional
    form: string,
    @optional
    formAction: string /* uri */,
    @optional
    formTarget: string /* "_blank", "_self", etc. */,
    @optional
    formMethod: string /* "post", "get", "put" */,
    @optional
    headers: string,
    @optional
    height: string /* in html5 this can only be a number, but in html4 it can ba a percentage as well */,
    @optional
    high: int,
    @optional
    href: string /* uri */,
    @optional
    hrefLang: string,
    @optional
    htmlFor: string /* substitute for "for" */,
    @optional
    httpEquiv: string /* has a fixed set of possible values */,
    @optional
    icon: string /* uri? */,
    @optional
    inputMode: string /* "verbatim", "latin", "numeric", etc. */,
    @optional
    integrity: string,
    @optional
    keyType: string,
    @optional
    kind: string /* has a fixed set of possible values */,
    @optional
    label: string,
    @optional
    list: string,
    @optional
    loop: bool,
    @optional
    low: int,
    @optional
    manifest: string /* uri */,
    @optional
    max: string /* should be int or Js.Date.t */,
    @optional
    maxLength: int,
    @optional
    media: string /* a valid media query */,
    @optional
    mediaGroup: string,
    @optional
    method: string /* "post" or "get" */,
    @optional
    min: string,
    @optional
    minLength: int,
    @optional
    multiple: bool,
    @optional
    muted: bool,
    @optional
    name: string,
    @optional
    nonce: string,
    @optional
    noValidate: bool,
    @optional @as("open")
    open_: bool /* use this one. Previous one is deprecated */,
    @optional
    optimum: int,
    @optional
    pattern: string /* valid Js RegExp */,
    @optional
    placeholder: string,
    @optional
    playsInline: bool,
    @optional
    poster: string /* uri */,
    @optional
    preload: string /* "none", "metadata" or "auto" (and "" as a synonym for "auto") */,
    @optional
    radioGroup: string,
    @optional
    readOnly: bool,
    @optional
    rel: string /* a space- or comma-separated (depending on the element) list of a fixed set of "link types" */,
    @optional
    required: bool,
    @optional
    reversed: bool,
    @optional
    rows: int,
    @optional
    rowSpan: int,
    @optional
    sandbox: string /* has a fixed set of possible values */,
    @optional
    scope: string /* has a fixed set of possible values */,
    @optional
    scoped: bool,
    @optional
    scrolling: string /* html4 only, "auto", "yes" or "no" */,
    /* seamless - supported by React, but removed from the html5 spec */
    @optional
    selected: bool,
    @optional
    shape: string,
    @optional
    size: int,
    @optional
    sizes: string,
    @optional
    span: int,
    @optional
    src: string /* uri */,
    @optional
    srcDoc: string,
    @optional
    srcLang: string,
    @optional
    srcSet: string,
    @optional
    start: int,
    @optional
    step: float,
    @optional
    summary: string /* deprecated */,
    @optional
    target: string,
    @optional @as("type")
    type_: string /* has a fixed but large-ish set of possible values */ /* use this one. Previous one is deprecated */,
    @optional
    useMap: string,
    @optional
    value: string,
    @optional
    width: string /* in html5 this can only be a number, but in html4 it can ba a percentage as well */,
    @optional
    wrap: string /* "hard" or "soft" */,
    /* Clipboard events */
    @optional
    onCopy: ReactEvent.Clipboard.t => unit,
    @optional
    onCut: ReactEvent.Clipboard.t => unit,
    @optional
    onPaste: ReactEvent.Clipboard.t => unit,
    /* Composition events */
    @optional
    onCompositionEnd: ReactEvent.Composition.t => unit,
    @optional
    onCompositionStart: ReactEvent.Composition.t => unit,
    @optional
    onCompositionUpdate: ReactEvent.Composition.t => unit,
    /* Keyboard events */
    @optional
    onKeyDown: ReactEvent.Keyboard.t => unit,
    @optional
    onKeyPress: ReactEvent.Keyboard.t => unit,
    @optional
    onKeyUp: ReactEvent.Keyboard.t => unit,
    /* Focus events */
    @optional
    onFocus: ReactEvent.Focus.t => unit,
    @optional
    onBlur: ReactEvent.Focus.t => unit,
    /* Form events */
    @optional
    onChange: ReactEvent.Form.t => unit,
    @optional
    onInput: ReactEvent.Form.t => unit,
    @optional
    onSubmit: ReactEvent.Form.t => unit,
    @optional
    onInvalid: ReactEvent.Form.t => unit,
    /* Mouse events */
    @optional
    onClick: ReactEvent.Mouse.t => unit,
    @optional
    onContextMenu: ReactEvent.Mouse.t => unit,
    @optional
    onDoubleClick: ReactEvent.Mouse.t => unit,
    @optional
    onDrag: ReactEvent.Mouse.t => unit,
    @optional
    onDragEnd: ReactEvent.Mouse.t => unit,
    @optional
    onDragEnter: ReactEvent.Mouse.t => unit,
    @optional
    onDragExit: ReactEvent.Mouse.t => unit,
    @optional
    onDragLeave: ReactEvent.Mouse.t => unit,
    @optional
    onDragOver: ReactEvent.Mouse.t => unit,
    @optional
    onDragStart: ReactEvent.Mouse.t => unit,
    @optional
    onDrop: ReactEvent.Mouse.t => unit,
    @optional
    onMouseDown: ReactEvent.Mouse.t => unit,
    @optional
    onMouseEnter: ReactEvent.Mouse.t => unit,
    @optional
    onMouseLeave: ReactEvent.Mouse.t => unit,
    @optional
    onMouseMove: ReactEvent.Mouse.t => unit,
    @optional
    onMouseOut: ReactEvent.Mouse.t => unit,
    @optional
    onMouseOver: ReactEvent.Mouse.t => unit,
    @optional
    onMouseUp: ReactEvent.Mouse.t => unit,
    /* Selection events */
    @optional
    onSelect: ReactEvent.Selection.t => unit,
    /* Touch events */
    @optional
    onTouchCancel: ReactEvent.Touch.t => unit,
    @optional
    onTouchEnd: ReactEvent.Touch.t => unit,
    @optional
    onTouchMove: ReactEvent.Touch.t => unit,
    @optional
    onTouchStart: ReactEvent.Touch.t => unit,
    // Pointer events
    @optional
    onPointerOver: ReactEvent.Pointer.t => unit,
    @optional
    onPointerEnter: ReactEvent.Pointer.t => unit,
    @optional
    onPointerDown: ReactEvent.Pointer.t => unit,
    @optional
    onPointerMove: ReactEvent.Pointer.t => unit,
    @optional
    onPointerUp: ReactEvent.Pointer.t => unit,
    @optional
    onPointerCancel: ReactEvent.Pointer.t => unit,
    @optional
    onPointerOut: ReactEvent.Pointer.t => unit,
    @optional
    onPointerLeave: ReactEvent.Pointer.t => unit,
    @optional
    onGotPointerCapture: ReactEvent.Pointer.t => unit,
    @optional
    onLostPointerCapture: ReactEvent.Pointer.t => unit,
    /* UI events */
    @optional
    onScroll: ReactEvent.UI.t => unit,
    /* Wheel events */
    @optional
    onWheel: ReactEvent.Wheel.t => unit,
    /* Media events */
    @optional
    onAbort: ReactEvent.Media.t => unit,
    @optional
    onCanPlay: ReactEvent.Media.t => unit,
    @optional
    onCanPlayThrough: ReactEvent.Media.t => unit,
    @optional
    onDurationChange: ReactEvent.Media.t => unit,
    @optional
    onEmptied: ReactEvent.Media.t => unit,
    @optional
    onEncrypetd: ReactEvent.Media.t => unit,
    @optional
    onEnded: ReactEvent.Media.t => unit,
    @optional
    onError: ReactEvent.Media.t => unit,
    @optional
    onLoadedData: ReactEvent.Media.t => unit,
    @optional
    onLoadedMetadata: ReactEvent.Media.t => unit,
    @optional
    onLoadStart: ReactEvent.Media.t => unit,
    @optional
    onPause: ReactEvent.Media.t => unit,
    @optional
    onPlay: ReactEvent.Media.t => unit,
    @optional
    onPlaying: ReactEvent.Media.t => unit,
    @optional
    onProgress: ReactEvent.Media.t => unit,
    @optional
    onRateChange: ReactEvent.Media.t => unit,
    @optional
    onSeeked: ReactEvent.Media.t => unit,
    @optional
    onSeeking: ReactEvent.Media.t => unit,
    @optional
    onStalled: ReactEvent.Media.t => unit,
    @optional
    onSuspend: ReactEvent.Media.t => unit,
    @optional
    onTimeUpdate: ReactEvent.Media.t => unit,
    @optional
    onVolumeChange: ReactEvent.Media.t => unit,
    @optional
    onWaiting: ReactEvent.Media.t => unit,
    /* Image events */
    @optional
    onLoad: ReactEvent.Image.t => unit /* duplicate */ /* ~onError: ReactEvent.Image.t => unit=?, */,
    /* Animation events */
    @optional
    onAnimationStart: ReactEvent.Animation.t => unit,
    @optional
    onAnimationEnd: ReactEvent.Animation.t => unit,
    @optional
    onAnimationIteration: ReactEvent.Animation.t => unit,
    /* Transition events */
    @optional
    onTransitionEnd: ReactEvent.Transition.t => unit,
    /* svg */
    @optional
    accentHeight: string,
    @optional
    accumulate: string,
    @optional
    additive: string,
    @optional
    alignmentBaseline: string,
    @optional
    allowReorder: string,
    @optional
    alphabetic: string,
    @optional
    amplitude: string,
    @optional
    arabicForm: string,
    @optional
    ascent: string,
    @optional
    attributeName: string,
    @optional
    attributeType: string,
    @optional
    autoReverse: string,
    @optional
    azimuth: string,
    @optional
    baseFrequency: string,
    @optional
    baseProfile: string,
    @optional
    baselineShift: string,
    @optional
    bbox: string,
    @optional @as("begin")
    begin_: string /* use this one. Previous one is deprecated */,
    @optional
    bias: string,
    @optional
    by: string,
    @optional
    calcMode: string,
    @optional
    capHeight: string,
    @optional
    clip: string,
    @optional
    clipPath: string,
    @optional
    clipPathUnits: string,
    @optional
    clipRule: string,
    @optional
    colorInterpolation: string,
    @optional
    colorInterpolationFilters: string,
    @optional
    colorProfile: string,
    @optional
    colorRendering: string,
    @optional
    contentScriptType: string,
    @optional
    contentStyleType: string,
    @optional
    cursor: string,
    @optional
    cx: string,
    @optional
    cy: string,
    @optional
    d: string,
    @optional
    decelerate: string,
    @optional
    descent: string,
    @optional
    diffuseConstant: string,
    @optional
    direction: string,
    @optional
    display: string,
    @optional
    divisor: string,
    @optional
    dominantBaseline: string,
    @optional
    dur: string,
    @optional
    dx: string,
    @optional
    dy: string,
    @optional
    edgeMode: string,
    @optional
    elevation: string,
    @optional
    enableBackground: string,
    @optional @as("end")
    end_: string /* use this one. Previous one is deprecated */,
    @optional
    exponent: string,
    @optional
    externalResourcesRequired: string,
    @optional
    fill: string,
    @optional
    fillOpacity: string,
    @optional
    fillRule: string,
    @optional
    filter: string,
    @optional
    filterRes: string,
    @optional
    filterUnits: string,
    @optional
    floodColor: string,
    @optional
    floodOpacity: string,
    @optional
    focusable: string,
    @optional
    fontFamily: string,
    @optional
    fontSize: string,
    @optional
    fontSizeAdjust: string,
    @optional
    fontStretch: string,
    @optional
    fontStyle: string,
    @optional
    fontVariant: string,
    @optional
    fontWeight: string,
    @optional
    fomat: string,
    @optional
    from: string,
    @optional
    fx: string,
    @optional
    fy: string,
    @optional
    g1: string,
    @optional
    g2: string,
    @optional
    glyphName: string,
    @optional
    glyphOrientationHorizontal: string,
    @optional
    glyphOrientationVertical: string,
    @optional
    glyphRef: string,
    @optional
    gradientTransform: string,
    @optional
    gradientUnits: string,
    @optional
    hanging: string,
    @optional
    horizAdvX: string,
    @optional
    horizOriginX: string,
    @optional
    ideographic: string,
    @optional
    imageRendering: string,
    @optional @as("in")
    in_: string /* use this one. Previous one is deprecated */,
    @optional
    in2: string,
    @optional
    intercept: string,
    @optional
    k: string,
    @optional
    k1: string,
    @optional
    k2: string,
    @optional
    k3: string,
    @optional
    k4: string,
    @optional
    kernelMatrix: string,
    @optional
    kernelUnitLength: string,
    @optional
    kerning: string,
    @optional
    keyPoints: string,
    @optional
    keySplines: string,
    @optional
    keyTimes: string,
    @optional
    lengthAdjust: string,
    @optional
    letterSpacing: string,
    @optional
    lightingColor: string,
    @optional
    limitingConeAngle: string,
    @optional
    local: string,
    @optional
    markerEnd: string,
    @optional
    markerHeight: string,
    @optional
    markerMid: string,
    @optional
    markerStart: string,
    @optional
    markerUnits: string,
    @optional
    markerWidth: string,
    @optional
    mask: string,
    @optional
    maskContentUnits: string,
    @optional
    maskUnits: string,
    @optional
    mathematical: string,
    @optional
    mode: string,
    @optional
    numOctaves: string,
    @optional
    offset: string,
    @optional
    opacity: string,
    @optional
    operator: string,
    @optional
    order: string,
    @optional
    orient: string,
    @optional
    orientation: string,
    @optional
    origin: string,
    @optional
    overflow: string,
    @optional
    overflowX: string,
    @optional
    overflowY: string,
    @optional
    overlinePosition: string,
    @optional
    overlineThickness: string,
    @optional
    paintOrder: string,
    @optional
    panose1: string,
    @optional
    pathLength: string,
    @optional
    patternContentUnits: string,
    @optional
    patternTransform: string,
    @optional
    patternUnits: string,
    @optional
    pointerEvents: string,
    @optional
    points: string,
    @optional
    pointsAtX: string,
    @optional
    pointsAtY: string,
    @optional
    pointsAtZ: string,
    @optional
    preserveAlpha: string,
    @optional
    preserveAspectRatio: string,
    @optional
    primitiveUnits: string,
    @optional
    r: string,
    @optional
    radius: string,
    @optional
    refX: string,
    @optional
    refY: string,
    @optional
    renderingIntent: string,
    @optional
    repeatCount: string,
    @optional
    repeatDur: string,
    @optional
    requiredExtensions: string,
    @optional
    requiredFeatures: string,
    @optional
    restart: string,
    @optional
    result: string,
    @optional
    rotate: string,
    @optional
    rx: string,
    @optional
    ry: string,
    @optional
    scale: string,
    @optional
    seed: string,
    @optional
    shapeRendering: string,
    @optional
    slope: string,
    @optional
    spacing: string,
    @optional
    specularConstant: string,
    @optional
    specularExponent: string,
    @optional
    speed: string,
    @optional
    spreadMethod: string,
    @optional
    startOffset: string,
    @optional
    stdDeviation: string,
    @optional
    stemh: string,
    @optional
    stemv: string,
    @optional
    stitchTiles: string,
    @optional
    stopColor: string,
    @optional
    stopOpacity: string,
    @optional
    strikethroughPosition: string,
    @optional
    strikethroughThickness: string,
    @optional
    string: string,
    @optional
    stroke: string,
    @optional
    strokeDasharray: string,
    @optional
    strokeDashoffset: string,
    @optional
    strokeLinecap: string,
    @optional
    strokeLinejoin: string,
    @optional
    strokeMiterlimit: string,
    @optional
    strokeOpacity: string,
    @optional
    strokeWidth: string,
    @optional
    surfaceScale: string,
    @optional
    systemLanguage: string,
    @optional
    tableValues: string,
    @optional
    targetX: string,
    @optional
    targetY: string,
    @optional
    textAnchor: string,
    @optional
    textDecoration: string,
    @optional
    textLength: string,
    @optional
    textRendering: string,
    @optional @as("to")
    to_: string /* use this one. Previous one is deprecated */,
    @optional
    transform: string,
    @optional
    u1: string,
    @optional
    u2: string,
    @optional
    underlinePosition: string,
    @optional
    underlineThickness: string,
    @optional
    unicode: string,
    @optional
    unicodeBidi: string,
    @optional
    unicodeRange: string,
    @optional
    unitsPerEm: string,
    @optional
    vAlphabetic: string,
    @optional
    vHanging: string,
    @optional
    vIdeographic: string,
    @optional
    vMathematical: string,
    @optional
    values: string,
    @optional
    vectorEffect: string,
    @optional
    version: string,
    @optional
    vertAdvX: string,
    @optional
    vertAdvY: string,
    @optional
    vertOriginX: string,
    @optional
    vertOriginY: string,
    @optional
    viewBox: string,
    @optional
    viewTarget: string,
    @optional
    visibility: string,
    /* width::string? => */
    @optional
    widths: string,
    @optional
    wordSpacing: string,
    @optional
    writingMode: string,
    @optional
    x: string,
    @optional
    x1: string,
    @optional
    x2: string,
    @optional
    xChannelSelector: string,
    @optional
    xHeight: string,
    @optional
    xlinkActuate: string,
    @optional
    xlinkArcrole: string,
    @optional
    xlinkHref: string,
    @optional
    xlinkRole: string,
    @optional
    xlinkShow: string,
    @optional
    xlinkTitle: string,
    @optional
    xlinkType: string,
    @optional
    xmlns: string,
    @optional
    xmlnsXlink: string,
    @optional
    xmlBase: string,
    @optional
    xmlLang: string,
    @optional
    xmlSpace: string,
    @optional
    y: string,
    @optional
    y1: string,
    @optional
    y2: string,
    @optional
    yChannelSelector: string,
    @optional
    z: string,
    @optional
    zoomAndPan: string,
    /* RDFa */
    @optional
    about: string,
    @optional
    datatype: string,
    @optional
    inlist: string,
    @optional
    prefix: string,
    @optional
    property: string,
    @optional
    resource: string,
    @optional
    typeof: string,
    @optional
    vocab: string,
    /* react-specific */
    @optional
    dangerouslySetInnerHTML: {"__html": string},
    @optional
    suppressContentEditableWarning: bool,
*)
    let mkLabel_ name typString =
      mkItem ~name ~kind:4 ~deprecated:None ~detail:typString ~docstring:[]
    in
    let mkLabel (name, typ) = mkLabel_ name typ in
    let keyLabel = mkLabel_ "key" "string" in
    if labels = [] then []
    else
      (labels
      |> List.filter (fun (name, _t) ->
             Utils.startsWith name prefix && not (List.mem name identsSeen))
      |> List.map mkLabel)
      @ [keyLabel]
  | Cjsx (componentPath, prefix, identsSeen) ->
    let items = findItems ~exact:true (componentPath @ ["make"]) in
    let labels =
      match items with
      | {SharedTypes.item = Value typ} :: _ ->
        let rec getFields (texp : Types.type_expr) =
          match texp.desc with
          | Tfield (name, _, t1, t2) ->
            let fields = t2 |> getFields in
            (name, t1) :: fields
          | Tlink te -> te |> getFields
          | Tvar None -> []
          | _ -> []
        in
        let rec getLabels (t : Types.type_expr) =
          match t.desc with
          | Tlink t1 | Tsubst t1 -> getLabels t1
          | Tarrow
              ( Nolabel,
                {
                  desc =
                    ( Tconstr (* Js.t *) (_, [{desc = Tobject (tObj, _)}], _)
                    | Tobject (tObj, _) );
                },
                _,
                _ ) ->
            getFields tObj
          | _ -> []
        in
        typ |> getLabels
      | _ -> []
    in
    let mkLabel_ name typString =
      mkItem ~name ~kind:4 ~deprecated:None ~detail:typString ~docstring:[]
    in
    let mkLabel (name, typ) = mkLabel_ name (typ |> Shared.typeToString) in
    let keyLabel = mkLabel_ "key" "string" in
    if labels = [] then []
    else
      (labels
      |> List.filter (fun (name, _t) ->
             Utils.startsWith name prefix && not (List.mem name identsSeen))
      |> List.map mkLabel)
      @ [keyLabel]
  | Cpath parts ->
    let items = parts |> findItems ~exact:false in
    (* TODO(#107): figure out why we're getting duplicates. *)
    items |> Utils.dedup
    |> List.map
         (fun {SharedTypes.name = {txt = name}; deprecated; docstring; item} ->
           mkItem ~name ~kind:(kindToInt item) ~deprecated
             ~detail:(detail name item) ~docstring)
  | Cpipe (pipe, partialName) -> (
    let arrayModulePath = ["Js"; "Array2"] in
    let listModulePath = ["Belt"; "List"] in
    let optionModulePath = ["Belt"; "Option"] in
    let stringModulePath = ["Js"; "String2"] in
    let getModulePath path =
      let rec loop (path : Path.t) =
        match path with
        | Pident id -> [Ident.name id]
        | Pdot (p, s, _) -> s :: loop p
        | Papply _ -> []
      in
      match path with
      | Path.Pident id when Ident.name id = "array" -> arrayModulePath
      | Path.Pident id when Ident.name id = "list" -> listModulePath
      | Path.Pident id when Ident.name id = "option" -> optionModulePath
      | Path.Pident id when Ident.name id = "string" -> stringModulePath
      | _ -> ( match loop path with _ :: rest -> List.rev rest | [] -> [])
    in
    let getLhsPath ~pipeId ~partialName =
      match [pipeId] |> findItems ~exact:true with
      | {SharedTypes.item = Value t} :: _ ->
        let modulePath =
          match t.desc with
          | Tconstr (path, _, _) -> getModulePath path
          | Tlink {desc = Tconstr (path, _, _)} -> getModulePath path
          | _ -> []
        in
        Some (modulePath, partialName)
      | _ -> None
    in
    let lhsPath =
      match pipe with
      | PipeId pipeId -> getLhsPath ~pipeId ~partialName
      | PipeString -> Some (stringModulePath, partialName)
      | PipeArray -> Some (arrayModulePath, partialName)
    in
    let removePackageOpens modulePath =
      match modulePath with
      | toplevel :: rest when package.opens |> List.mem toplevel -> rest
      | _ -> modulePath
    in
    let rec removeRawOpen rawOpen modulePath =
      match (rawOpen, modulePath) with
      | Tip _, _ -> Some modulePath
      | Nested (s, inner), first :: restPath when s = first ->
        removeRawOpen inner restPath
      | _ -> None
    in
    let rec removeRawOpens rawOpens modulePath =
      match rawOpens with
      | rawOpen :: restOpens ->
        let newModulePath =
          match removeRawOpen rawOpen modulePath with
          | None -> modulePath
          | Some newModulePath -> newModulePath
        in
        removeRawOpens restOpens newModulePath
      | [] -> modulePath
    in
    match lhsPath with
    | Some (modulePath, partialName) -> (
      match modulePath with
      | _ :: _ ->
        let modulePathMinusOpens =
          modulePath |> removePackageOpens |> removeRawOpens rawOpens
          |> String.concat "."
        in
        let completionName name =
          if modulePathMinusOpens = "" then name
          else modulePathMinusOpens ^ "." ^ name
        in
        let parts = modulePath @ [partialName] in
        let items = parts |> findItems ~exact:false in
        items
        |> List.filter (fun {item} ->
               match item with Value _ -> true | _ -> false)
        |> List.map
             (fun {SharedTypes.name = {txt = name}; deprecated; docstring; item}
             ->
               mkItem ~name:(completionName name) ~kind:(kindToInt item)
                 ~detail:(detail name item) ~deprecated ~docstring)
      | _ -> [])
    | None -> [])
  | Cdecorator prefix ->
    let mkDecorator name =
      mkItem ~name ~kind:4 ~deprecated:None ~detail:"" ~docstring:[]
    in
    [
      "as";
      "deriving";
      "genType";
      "genType.as";
      "genType.import";
      "genType.opaque";
      "get";
      "get_index";
      "inline";
      "int";
      "meth";
      "module";
      "new";
      "obj";
      "react.component";
      "return";
      "scope";
      "send";
      "set";
      "set_index";
      "string";
      "this";
      "unboxed";
      "uncurry";
      "unwrap";
      "val";
      "variadic";
    ]
    |> List.filter (fun decorator -> Utils.startsWith decorator prefix)
    |> List.map (fun decorator ->
           let parts = String.split_on_char '.' prefix in
           let len = String.length prefix in
           let dec2 =
             if List.length parts > 1 then
               String.sub decorator len (String.length decorator - len)
             else decorator
           in
           dec2)
    |> List.map mkDecorator
  | Clabel (funPath, prefix, identsSeen) ->
    let labels =
      match funPath |> findItems ~exact:true with
      | {SharedTypes.item = Value typ} :: _ ->
        let rec getLabels (t : Types.type_expr) =
          match t.desc with
          | Tlink t1 | Tsubst t1 -> getLabels t1
          | Tarrow ((Labelled l | Optional l), tArg, tRet, _) ->
            (l, tArg) :: getLabels tRet
          | Tarrow (Nolabel, _, tRet, _) -> getLabels tRet
          | _ -> []
        in
        typ |> getLabels
      | _ -> []
    in
    let mkLabel (name, typ) =
      mkItem ~name ~kind:4 ~deprecated:None
        ~detail:(typ |> Shared.typeToString)
        ~docstring:[]
    in
    labels
    |> List.filter (fun (name, _t) ->
           Utils.startsWith name prefix && not (List.mem name identsSeen))
    |> List.map mkLabel

let computeCompletions ~full ~maybeText ~pos =
  let package = full.package in
  match maybeText with
  | None -> []
  | Some text -> (
    match PartialParser.positionToOffset text pos with
    | None -> []
    | Some offset -> (
      match PartialParser.findCompletable text offset with
      | None -> []
      | Some completable ->
        let rawOpens = PartialParser.findOpens text offset in
        let allModules = package.localModules @ package.dependencyModules in
        let findItems ~exact parts =
          let items =
            getItems ~full ~package ~rawOpens ~allModules ~pos ~parts
          in
          match parts |> List.rev with
          | last :: _ when exact ->
            items |> List.filter (fun {SharedTypes.name = {txt}} -> txt = last)
          | _ -> items
        in
        completable |> processCompletable ~findItems ~package ~rawOpens))
