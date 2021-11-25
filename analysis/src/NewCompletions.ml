open SharedTypes

let domLabels =
  [
    ("ariaDetails", "string");
    ("ariaDisabled", "bool");
    ("ariaHidden", "bool");
    ("ariaKeyshortcuts", "string");
    ("ariaLabel", "string");
    ("ariaRoledescription", "string");
    ("ariaExpanded", "bool");
    ("ariaLevel", "int");
    ("ariaModal", "bool");
    ("ariaMultiline", "bool");
    ("ariaMultiselectable", "bool");
    ("ariaPlaceholder", "string");
    ("ariaReadonly", "bool");
    ("ariaRequired", "bool");
    ("ariaSelected", "bool");
    ("ariaSort", "string");
    ("ariaValuemax", "float");
    ("ariaValuemin", "float");
    ("ariaValuenow", "float");
    ("ariaValuetext", "string");
    ("ariaAtomic", "bool");
    ("ariaBusy", "bool");
    ("ariaRelevant", "string");
    ("ariaGrabbed", "bool");
    ("ariaActivedescendant", "string");
    ("ariaColcount", "int");
    ("ariaColindex", "int");
    ("ariaColspan", "int");
    ("ariaControls", "string");
    ("ariaDescribedby", "string");
    ("ariaErrormessage", "string");
    ("ariaFlowto", "string");
    ("ariaLabelledby", "string");
    ("ariaOwns", "string");
    ("ariaPosinset", "int");
    ("ariaRowcount", "int");
    ("ariaRowindex", "int");
    ("ariaRowspan", "int");
    ("ariaSetsize", "int");
    ("defaultChecked", "bool");
    ("defaultValue", "string");
    ("accessKey", "string");
    ("contentEditable", "bool");
    ("contextMenu", "string");
    ("draggable", "bool");
    ("hidden", "bool");
    ("id", "string");
    ("lang", "string");
    ("style", "style");
    ("spellCheck", "bool");
    ("tabIndex", "int");
    ("title", "string");
    ("itemID", "string");
    ("itemProp", "string");
    ("itemRef", "string");
    ("itemScope", "bool");
    ("accept", "string");
    ("acceptCharset", "string");
    ("allowFullScreen", "bool");
    ("alt", "string");
    ("async", "bool");
    ("autoFocus", "bool");
    ("autoPlay", "bool");
    ("challenge", "string");
    ("charSet", "string");
    ("checked", "bool");
    ("cols", "int");
    ("colSpan", "int");
    ("content", "string");
    ("controls", "bool");
    ("default", "bool");
    ("defer", "bool");
    ("disabled", "bool");
    ("form", "string");
    ("headers", "string");
    ("high", "int");
    ("hrefLang", "string");
    ("integrity", "string");
    ("keyType", "string");
    ("label", "string");
    ("list", "string");
    ("loop", "bool");
    ("low", "int");
    ("maxLength", "int");
    ("mediaGroup", "string");
    ("min", "string");
    ("minLength", "int");
    ("multiple", "bool");
    ("muted", "bool");
    ("name", "string");
    ("nonce", "string");
    ("noValidate", "bool");
    ("optimum", "int");
    ("placeholder", "string");
    ("playsInline", "bool");
    ("radioGroup", "string");
    ("readOnly", "bool");
    ("required", "bool");
    ("reversed", "bool");
    ("rows", "int");
    ("rowSpan", "int");
    ("scoped", "bool");
    ("selected", "bool");
    ("shape", "string");
    ("size", "int");
    ("sizes", "string");
    ("span", "int");
    ("srcDoc", "string");
    ("srcLang", "string");
    ("srcSet", "string");
    ("start", "int");
    ("step", "float");
    ("target", "string");
    ("useMap", "string");
    ("value", "string");
    ("onCopy", "ReactEvent.Clipboard.t => unit");
    ("onCut", "ReactEvent.Clipboard.t => unit");
    ("onPaste", "ReactEvent.Clipboard.t => unit");
    ("onCompositionEnd", "ReactEvent.Composition.t => unit");
    ("onCompositionStart", "ReactEvent.Composition.t => unit");
    ("onCompositionUpdate", "ReactEvent.Composition.t => unit");
    ("onKeyDown", "ReactEvent.Keyboard.t => unit");
    ("onKeyPress", "ReactEvent.Keyboard.t => unit");
    ("onKeyUp", "ReactEvent.Keyboard.t => unit");
    ("onFocus", "ReactEvent.Focus.t => unit");
    ("onBlur", "ReactEvent.Focus.t => unit");
    ("onChange", "ReactEvent.Form.t => unit");
    ("onInput", "ReactEvent.Form.t => unit");
    ("onSubmit", "ReactEvent.Form.t => unit");
    ("onInvalid", "ReactEvent.Form.t => unit");
    ("onClick", "ReactEvent.Mouse.t => unit");
    ("onContextMenu", "ReactEvent.Mouse.t => unit");
    ("onDoubleClick", "ReactEvent.Mouse.t => unit");
    ("onDrag", "ReactEvent.Mouse.t => unit");
    ("onDragEnd", "ReactEvent.Mouse.t => unit");
    ("onDragEnter", "ReactEvent.Mouse.t => unit");
    ("onDragExit", "ReactEvent.Mouse.t => unit");
    ("onDragLeave", "ReactEvent.Mouse.t => unit");
    ("onDragOver", "ReactEvent.Mouse.t => unit");
    ("onDragStart", "ReactEvent.Mouse.t => unit");
    ("onDrop", "ReactEvent.Mouse.t => unit");
    ("onMouseDown", "ReactEvent.Mouse.t => unit");
    ("onMouseEnter", "ReactEvent.Mouse.t => unit");
    ("onMouseLeave", "ReactEvent.Mouse.t => unit");
    ("onMouseMove", "ReactEvent.Mouse.t => unit");
    ("onMouseOut", "ReactEvent.Mouse.t => unit");
    ("onMouseOver", "ReactEvent.Mouse.t => unit");
    ("onMouseUp", "ReactEvent.Mouse.t => unit");
    ("onSelect", "ReactEvent.Selection.t => unit");
    ("onTouchCancel", "ReactEvent.Touch.t => unit");
    ("onTouchEnd", "ReactEvent.Touch.t => unit");
    ("onTouchMove", "ReactEvent.Touch.t => unit");
    ("onTouchStart", "ReactEvent.Touch.t => unit");
    ("onPointerOver", "ReactEvent.Pointer.t => unit");
    ("onPointerEnter", "ReactEvent.Pointer.t => unit");
    ("onPointerDown", "ReactEvent.Pointer.t => unit");
    ("onPointerMove", "ReactEvent.Pointer.t => unit");
    ("onPointerUp", "ReactEvent.Pointer.t => unit");
    ("onPointerCancel", "ReactEvent.Pointer.t => unit");
    ("onPointerOut", "ReactEvent.Pointer.t => unit");
    ("onPointerLeave", "ReactEvent.Pointer.t => unit");
    ("onGotPointerCapture", "ReactEvent.Pointer.t => unit");
    ("onLostPointerCapture", "ReactEvent.Pointer.t => unit");
    ("onScroll", "ReactEvent.UI.t => unit");
    ("onWheel", "ReactEvent.Wheel.t => unit");
    ("onAbort", "ReactEvent.Media.t => unit");
    ("onCanPlay", "ReactEvent.Media.t => unit");
    ("onCanPlayThrough", "ReactEvent.Media.t => unit");
    ("onDurationChange", "ReactEvent.Media.t => unit");
    ("onEmptied", "ReactEvent.Media.t => unit");
    ("onEncrypetd", "ReactEvent.Media.t => unit");
    ("onEnded", "ReactEvent.Media.t => unit");
    ("onError", "ReactEvent.Media.t => unit");
    ("onLoadedData", "ReactEvent.Media.t => unit");
    ("onLoadedMetadata", "ReactEvent.Media.t => unit");
    ("onLoadStart", "ReactEvent.Media.t => unit");
    ("onPause", "ReactEvent.Media.t => unit");
    ("onPlay", "ReactEvent.Media.t => unit");
    ("onPlaying", "ReactEvent.Media.t => unit");
    ("onProgress", "ReactEvent.Media.t => unit");
    ("onRateChange", "ReactEvent.Media.t => unit");
    ("onSeeked", "ReactEvent.Media.t => unit");
    ("onSeeking", "ReactEvent.Media.t => unit");
    ("onStalled", "ReactEvent.Media.t => unit");
    ("onSuspend", "ReactEvent.Media.t => unit");
    ("onTimeUpdate", "ReactEvent.Media.t => unit");
    ("onVolumeChange", "ReactEvent.Media.t => unit");
    ("onWaiting", "ReactEvent.Media.t => unit");
    ("onAnimationStart", "ReactEvent.Animation.t => unit");
    ("onAnimationEnd", "ReactEvent.Animation.t => unit");
    ("onAnimationIteration", "ReactEvent.Animation.t => unit");
    ("onTransitionEnd", "ReactEvent.Transition.t => unit");
    ("accentHeight", "string");
    ("accumulate", "string");
    ("additive", "string");
    ("alignmentBaseline", "string");
    ("allowReorder", "string");
    ("alphabetic", "string");
    ("amplitude", "string");
    ("arabicForm", "string");
    ("ascent", "string");
    ("attributeName", "string");
    ("attributeType", "string");
    ("autoReverse", "string");
    ("azimuth", "string");
    ("baseFrequency", "string");
    ("baseProfile", "string");
    ("baselineShift", "string");
    ("bbox", "string");
    ("bias", "string");
    ("by", "string");
    ("calcMode", "string");
    ("capHeight", "string");
    ("clip", "string");
    ("clipPath", "string");
    ("clipPathUnits", "string");
    ("clipRule", "string");
    ("colorInterpolation", "string");
    ("colorInterpolationFilters", "string");
    ("colorProfile", "string");
    ("colorRendering", "string");
    ("contentScriptType", "string");
    ("contentStyleType", "string");
    ("cursor", "string");
    ("cx", "string");
    ("cy", "string");
    ("d", "string");
    ("decelerate", "string");
    ("descent", "string");
    ("diffuseConstant", "string");
    ("direction", "string");
    ("display", "string");
    ("divisor", "string");
    ("dominantBaseline", "string");
    ("dur", "string");
    ("dx", "string");
    ("dy", "string");
    ("edgeMode", "string");
    ("elevation", "string");
    ("enableBackground", "string");
    ("exponent", "string");
    ("externalResourcesRequired", "string");
    ("fill", "string");
    ("fillOpacity", "string");
    ("fillRule", "string");
    ("filter", "string");
    ("filterRes", "string");
    ("filterUnits", "string");
    ("floodColor", "string");
    ("floodOpacity", "string");
    ("focusable", "string");
    ("fontFamily", "string");
    ("fontSize", "string");
    ("fontSizeAdjust", "string");
    ("fontStretch", "string");
    ("fontStyle", "string");
    ("fontVariant", "string");
    ("fontWeight", "string");
    ("fomat", "string");
    ("from", "string");
    ("fx", "string");
    ("fy", "string");
    ("g1", "string");
    ("g2", "string");
    ("glyphName", "string");
    ("glyphOrientationHorizontal", "string");
    ("glyphOrientationVertical", "string");
    ("glyphRef", "string");
    ("gradientTransform", "string");
    ("gradientUnits", "string");
    ("hanging", "string");
    ("horizAdvX", "string");
    ("horizOriginX", "string");
    ("ideographic", "string");
    ("imageRendering", "string");
    ("in2", "string");
    ("intercept", "string");
    ("k", "string");
    ("k1", "string");
    ("k2", "string");
    ("k3", "string");
    ("k4", "string");
    ("kernelMatrix", "string");
    ("kernelUnitLength", "string");
    ("kerning", "string");
    ("keyPoints", "string");
    ("keySplines", "string");
    ("keyTimes", "string");
    ("lengthAdjust", "string");
    ("letterSpacing", "string");
    ("lightingColor", "string");
    ("limitingConeAngle", "string");
    ("local", "string");
    ("markerEnd", "string");
    ("markerHeight", "string");
    ("markerMid", "string");
    ("markerStart", "string");
    ("markerUnits", "string");
    ("markerWidth", "string");
    ("mask", "string");
    ("maskContentUnits", "string");
    ("maskUnits", "string");
    ("mathematical", "string");
    ("mode", "string");
    ("numOctaves", "string");
    ("offset", "string");
    ("opacity", "string");
    ("operator", "string");
    ("order", "string");
    ("orient", "string");
    ("orientation", "string");
    ("origin", "string");
    ("overflow", "string");
    ("overflowX", "string");
    ("overflowY", "string");
    ("overlinePosition", "string");
    ("overlineThickness", "string");
    ("paintOrder", "string");
    ("panose1", "string");
    ("pathLength", "string");
    ("patternContentUnits", "string");
    ("patternTransform", "string");
    ("patternUnits", "string");
    ("pointerEvents", "string");
    ("points", "string");
    ("pointsAtX", "string");
    ("pointsAtY", "string");
    ("pointsAtZ", "string");
    ("preserveAlpha", "string");
    ("preserveAspectRatio", "string");
    ("primitiveUnits", "string");
    ("r", "string");
    ("radius", "string");
    ("refX", "string");
    ("refY", "string");
    ("renderingIntent", "string");
    ("repeatCount", "string");
    ("repeatDur", "string");
    ("requiredExtensions", "string");
    ("requiredFeatures", "string");
    ("restart", "string");
    ("result", "string");
    ("rotate", "string");
    ("rx", "string");
    ("ry", "string");
    ("scale", "string");
    ("seed", "string");
    ("shapeRendering", "string");
    ("slope", "string");
    ("spacing", "string");
    ("specularConstant", "string");
    ("specularExponent", "string");
    ("speed", "string");
    ("spreadMethod", "string");
    ("startOffset", "string");
    ("stdDeviation", "string");
    ("stemh", "string");
    ("stemv", "string");
    ("stitchTiles", "string");
    ("stopColor", "string");
    ("stopOpacity", "string");
    ("strikethroughPosition", "string");
    ("strikethroughThickness", "string");
    ("string", "string");
    ("stroke", "string");
    ("strokeDasharray", "string");
    ("strokeDashoffset", "string");
    ("strokeLinecap", "string");
    ("strokeLinejoin", "string");
    ("strokeMiterlimit", "string");
    ("strokeOpacity", "string");
    ("strokeWidth", "string");
    ("surfaceScale", "string");
    ("systemLanguage", "string");
    ("tableValues", "string");
    ("targetX", "string");
    ("targetY", "string");
    ("textAnchor", "string");
    ("textDecoration", "string");
    ("textLength", "string");
    ("textRendering", "string");
    ("transform", "string");
    ("u1", "string");
    ("u2", "string");
    ("underlinePosition", "string");
    ("underlineThickness", "string");
    ("unicode", "string");
    ("unicodeBidi", "string");
    ("unicodeRange", "string");
    ("unitsPerEm", "string");
    ("vAlphabetic", "string");
    ("vHanging", "string");
    ("vIdeographic", "string");
    ("vMathematical", "string");
    ("values", "string");
    ("vectorEffect", "string");
    ("version", "string");
    ("vertAdvX", "string");
    ("vertAdvY", "string");
    ("vertOriginX", "string");
    ("vertOriginY", "string");
    ("viewBox", "string");
    ("viewTarget", "string");
    ("visibility", "string");
    ("widths", "string");
    ("wordSpacing", "string");
    ("writingMode", "string");
    ("x", "string");
    ("x1", "string");
    ("x2", "string");
    ("xChannelSelector", "string");
    ("xHeight", "string");
    ("xlinkActuate", "string");
    ("xlinkArcrole", "string");
    ("xlinkHref", "string");
    ("xlinkRole", "string");
    ("xlinkShow", "string");
    ("xlinkTitle", "string");
    ("xlinkType", "string");
    ("xmlns", "string");
    ("xmlnsXlink", "string");
    ("xmlBase", "string");
    ("xmlLang", "string");
    ("xmlSpace", "string");
    ("y", "string");
    ("y1", "string");
    ("y2", "string");
    ("yChannelSelector", "string");
    ("z", "string");
    ("zoomAndPan", "string");
    ("about", "string");
    ("datatype", "string");
    ("inlist", "string");
    ("prefix", "string");
    ("property", "string");
    ("resource", "string");
    ("typeof", "string");
    ("vocab", "string");
    ("dangerouslySetInnerHTML", "{\"__html\": string}");
    ("suppressContentEditableWarning", "bool");
  ]

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

let getItems ~full ~rawOpens ~allFiles ~pos ~parts =
  Log.log
    ("Opens folkz > "
    ^ string_of_int (List.length rawOpens)
    ^ " "
    ^ String.concat " ... " (rawOpens |> List.map pathToString));
  let env = QueryEnv.fromFile full.file in
  let package = full.package in
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
      allFiles |> FileSet.elements
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
            match References.digConstructor ~env ~package path with
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
                               References.digConstructor ~env ~package path))
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

let processCompletable ~findItems ~full ~package ~rawOpens
    (completable : PartialParser.completable) =
  match completable with
  | Cjsx ([id], prefix, identsSeen) when String.lowercase_ascii id = id ->
    let mkLabel_ name typString =
      mkItem ~name ~kind:4 ~deprecated:None ~detail:typString ~docstring:[]
    in
    let mkLabel (name, typ) = mkLabel_ name typ in
    let keyLabels =
      if Utils.startsWith "key" prefix then [mkLabel_ "key" "string"] else []
    in
    if domLabels = [] then []
    else
      (domLabels
      |> List.filter (fun (name, _t) ->
             Utils.startsWith name prefix && not (List.mem name identsSeen))
      |> List.map mkLabel)
      @ keyLabels
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
          | Tconstr
              ( path,
                [
                  {
                    desc =
                      ( Tconstr (* Js.t *) (_, [{desc = Tobject (tObj, _)}], _)
                      | Tobject (tObj, _) );
                  };
                  _;
                ],
                _ )
            when Path.name path = "React.componentLike" ->
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
    let keyLabels =
      if Utils.startsWith "key" prefix then [mkLabel_ "key" "string"] else []
    in
    if labels = [] then []
    else
      (labels
      |> List.filter (fun (name, _t) ->
             Utils.startsWith name prefix && not (List.mem name identsSeen))
      |> List.map mkLabel)
      @ keyLabels
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
      let getConstr typ =
        match typ.Types.desc with
        | Tconstr (path, _, _)
        | Tlink {desc = Tconstr (path, _, _)}
        | Tpoly ({desc = Tconstr (path, _, _)}, []) ->
          Some path
        | _ -> None
      in
      let fromType typ =
        match getConstr typ with
        | None -> None
        | Some path ->
          let modulePath = getModulePath path in
          Some (modulePath, partialName)
      in
      let getField ~env ~typ fieldName =
        match getConstr typ with
        | Some path -> (
          match References.digConstructor ~env ~package path with
          | None -> None
          | Some (env1, declared) -> (
            let t = declared.item in
            match t.kind with
            | Record fields -> (
              match
                fields
                |> List.find_opt (fun field ->
                       field.SharedTypes.fname.txt = fieldName)
              with
              | None -> None
              | Some field -> Some (field.typ, env1))
            | _ -> None))
        | None -> None
      in
      let rec getFields ~env ~typ = function
        | [] -> Some (typ, env)
        | fieldName :: rest -> (
          match getField ~env ~typ fieldName with
          | None -> None
          | Some (typ1, env1) -> getFields ~env:env1 ~typ:typ1 rest)
      in
      match String.split_on_char '.' pipeId with
      | x :: fieldNames -> (
        match [x] |> findItems ~exact:true with
        | {SharedTypes.item = Value typ} :: _ -> (
          let env = QueryEnv.fromFile full.file in
          match getFields ~env ~typ fieldNames with
          | None -> None
          | Some (typ1, _env1) -> fromType typ1)
        | _ -> None)
      | [] -> None
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
  | Cobj (lhs, path, prefix) ->
    let rec getFields (texp : Types.type_expr) =
      match texp.desc with
      | Tfield (name, _, t1, t2) ->
        let fields = t2 |> getFields in
        (name, t1) :: fields
      | Tlink te -> te |> getFields
      | Tvar None -> []
      | _ -> []
    in
    let envRef = ref (QueryEnv.fromFile full.file) in
    let rec getObj (t : Types.type_expr) =
      match t.desc with
      | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> getObj t1
      | Tobject (tObj, _) -> getFields tObj
      | Tconstr (path, _, _) -> (
        match References.digConstructor ~env:envRef.contents ~package path with
        | Some (env, {item = {decl = {type_manifest = Some tt}}}) ->
          envRef := env;
          getObj tt
        | _ -> [])
      | _ -> []
    in
    let fields =
      match [lhs] |> findItems ~exact:true with
      | {SharedTypes.item = Value typ} :: _ -> getObj typ
      | _ -> []
    in
    let rec resolvePath fields path =
      match path with
      | name :: restPath -> (
        match fields |> List.find_opt (fun (n, _) -> n = name) with
        | Some (_, fieldType) ->
          let innerFields = getObj fieldType in
          resolvePath innerFields restPath
        | None -> [])
      | [] -> fields
    in
    let labels = resolvePath fields path in
    let mkLabel_ name typString =
      mkItem ~name ~kind:4 ~deprecated:None ~detail:typString ~docstring:[]
    in
    let mkLabel (name, typ) = mkLabel_ name (typ |> Shared.typeToString) in
    if labels = [] then []
    else
      labels
      |> List.filter (fun (name, _t) -> Utils.startsWith name prefix)
      |> List.map mkLabel

let getCompletable ~textOpt ~pos =
  match textOpt with
  | None -> None
  | Some text -> (
    match PartialParser.positionToOffset text pos with
    | None -> None
    | Some offset -> (
      match PartialParser.findCompletable text offset with
      | None -> None
      | Some completable ->
        let rawOpens = PartialParser.findOpens text offset in
        Some (completable, rawOpens)))

let computeCompletions ~completable ~full ~pos ~rawOpens =
  let package = full.package in
  let allFiles = FileSet.union package.projectFiles package.dependenciesFiles in
  let findItems ~exact parts =
    let items = getItems ~full ~rawOpens ~allFiles ~pos ~parts in
    match parts |> List.rev with
    | last :: _ when exact ->
      items |> List.filter (fun {SharedTypes.name = {txt}} -> txt = last)
    | _ -> items
  in
  completable |> processCompletable ~findItems ~full ~package ~rawOpens
