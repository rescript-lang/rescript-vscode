open SharedTypes

let domLabels =
  let bool = "bool" in
  let float = "float" in
  let int = "int" in
  let string = "string" in
  [
    ("ariaDetails", string);
    ("ariaDisabled", bool);
    ("ariaHidden", bool);
    ("ariaKeyshortcuts", string);
    ("ariaLabel", string);
    ("ariaRoledescription", string);
    ("ariaExpanded", bool);
    ("ariaLevel", int);
    ("ariaModal", bool);
    ("ariaMultiline", bool);
    ("ariaMultiselectable", bool);
    ("ariaPlaceholder", string);
    ("ariaReadonly", bool);
    ("ariaRequired", bool);
    ("ariaSelected", bool);
    ("ariaSort", string);
    ("ariaValuemax", float);
    ("ariaValuemin", float);
    ("ariaValuenow", float);
    ("ariaValuetext", string);
    ("ariaAtomic", bool);
    ("ariaBusy", bool);
    ("ariaRelevant", string);
    ("ariaGrabbed", bool);
    ("ariaActivedescendant", string);
    ("ariaColcount", int);
    ("ariaColindex", int);
    ("ariaColspan", int);
    ("ariaControls", string);
    ("ariaDescribedby", string);
    ("ariaErrormessage", string);
    ("ariaFlowto", string);
    ("ariaLabelledby", string);
    ("ariaOwns", string);
    ("ariaPosinset", int);
    ("ariaRowcount", int);
    ("ariaRowindex", int);
    ("ariaRowspan", int);
    ("ariaSetsize", int);
    ("defaultChecked", bool);
    ("defaultValue", string);
    ("accessKey", string);
    ("className", string);
    ("contentEditable", bool);
    ("contextMenu", string);
    ("dir", string);
    ("draggable", bool);
    ("hidden", bool);
    ("id", string);
    ("lang", string);
    ("style", "style");
    ("spellCheck", bool);
    ("tabIndex", int);
    ("title", string);
    ("itemID", string);
    ("itemProp", string);
    ("itemRef", string);
    ("itemScope", bool);
    ("itemType", string);
    ("accept", string);
    ("acceptCharset", string);
    ("action", string);
    ("allowFullScreen", bool);
    ("alt", string);
    ("async", bool);
    ("autoComplete", string);
    ("autoCapitalize", string);
    ("autoFocus", bool);
    ("autoPlay", bool);
    ("challenge", string);
    ("charSet", string);
    ("checked", bool);
    ("cite", string);
    ("crossOrigin", string);
    ("cols", int);
    ("colSpan", int);
    ("content", string);
    ("controls", bool);
    ("coords", string);
    ("data", string);
    ("dateTime", string);
    ("default", bool);
    ("defer", bool);
    ("disabled", bool);
    ("download", string);
    ("encType", string);
    ("form", string);
    ("formAction", string);
    ("formTarget", string);
    ("formMethod", string);
    ("headers", string);
    ("height", string);
    ("high", int);
    ("href", string);
    ("hrefLang", string);
    ("htmlFor", string);
    ("httpEquiv", string);
    ("icon", string);
    ("inputMode", string);
    ("integrity", string);
    ("keyType", string);
    ("label", string);
    ("list", string);
    ("loop", bool);
    ("low", int);
    ("manifest", string);
    ("max", string);
    ("maxLength", int);
    ("media", string);
    ("mediaGroup", string);
    ("method", string);
    ("min", string);
    ("minLength", int);
    ("multiple", bool);
    ("muted", bool);
    ("name", string);
    ("nonce", string);
    ("noValidate", bool);
    ("open_", bool);
    ("optimum", int);
    ("pattern", string);
    ("placeholder", string);
    ("playsInline", bool);
    ("poster", string);
    ("preload", string);
    ("radioGroup", string);
    ("readOnly", bool);
    ("rel", string);
    ("required", bool);
    ("reversed", bool);
    ("rows", int);
    ("rowSpan", int);
    ("sandbox", string);
    ("scope", string);
    ("scoped", bool);
    ("scrolling", string);
    ("selected", bool);
    ("shape", string);
    ("size", int);
    ("sizes", string);
    ("span", int);
    ("src", string);
    ("srcDoc", string);
    ("srcLang", string);
    ("srcSet", string);
    ("start", int);
    ("step", float);
    ("summary", string);
    ("target", string);
    ("type_", string);
    ("useMap", string);
    ("value", string);
    ("width", string);
    ("wrap", string);
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
    ("accentHeight", string);
    ("accumulate", string);
    ("additive", string);
    ("alignmentBaseline", string);
    ("allowReorder", string);
    ("alphabetic", string);
    ("amplitude", string);
    ("arabicForm", string);
    ("ascent", string);
    ("attributeName", string);
    ("attributeType", string);
    ("autoReverse", string);
    ("azimuth", string);
    ("baseFrequency", string);
    ("baseProfile", string);
    ("baselineShift", string);
    ("bbox", string);
    ("bias", string);
    ("by", string);
    ("calcMode", string);
    ("capHeight", string);
    ("clip", string);
    ("clipPath", string);
    ("clipPathUnits", string);
    ("clipRule", string);
    ("colorInterpolation", string);
    ("colorInterpolationFilters", string);
    ("colorProfile", string);
    ("colorRendering", string);
    ("contentScriptType", string);
    ("contentStyleType", string);
    ("cursor", string);
    ("cx", string);
    ("cy", string);
    ("d", string);
    ("decelerate", string);
    ("descent", string);
    ("diffuseConstant", string);
    ("direction", string);
    ("display", string);
    ("divisor", string);
    ("dominantBaseline", string);
    ("dur", string);
    ("dx", string);
    ("dy", string);
    ("edgeMode", string);
    ("elevation", string);
    ("enableBackground", string);
    ("exponent", string);
    ("externalResourcesRequired", string);
    ("fill", string);
    ("fillOpacity", string);
    ("fillRule", string);
    ("filter", string);
    ("filterRes", string);
    ("filterUnits", string);
    ("floodColor", string);
    ("floodOpacity", string);
    ("focusable", string);
    ("fontFamily", string);
    ("fontSize", string);
    ("fontSizeAdjust", string);
    ("fontStretch", string);
    ("fontStyle", string);
    ("fontVariant", string);
    ("fontWeight", string);
    ("fomat", string);
    ("from", string);
    ("fx", string);
    ("fy", string);
    ("g1", string);
    ("g2", string);
    ("glyphName", string);
    ("glyphOrientationHorizontal", string);
    ("glyphOrientationVertical", string);
    ("glyphRef", string);
    ("gradientTransform", string);
    ("gradientUnits", string);
    ("hanging", string);
    ("horizAdvX", string);
    ("horizOriginX", string);
    ("ideographic", string);
    ("imageRendering", string);
    ("in2", string);
    ("intercept", string);
    ("k", string);
    ("k1", string);
    ("k2", string);
    ("k3", string);
    ("k4", string);
    ("kernelMatrix", string);
    ("kernelUnitLength", string);
    ("kerning", string);
    ("keyPoints", string);
    ("keySplines", string);
    ("keyTimes", string);
    ("lengthAdjust", string);
    ("letterSpacing", string);
    ("lightingColor", string);
    ("limitingConeAngle", string);
    ("local", string);
    ("markerEnd", string);
    ("markerHeight", string);
    ("markerMid", string);
    ("markerStart", string);
    ("markerUnits", string);
    ("markerWidth", string);
    ("mask", string);
    ("maskContentUnits", string);
    ("maskUnits", string);
    ("mathematical", string);
    ("mode", string);
    ("numOctaves", string);
    ("offset", string);
    ("opacity", string);
    ("operator", string);
    ("order", string);
    ("orient", string);
    ("orientation", string);
    ("origin", string);
    ("overflow", string);
    ("overflowX", string);
    ("overflowY", string);
    ("overlinePosition", string);
    ("overlineThickness", string);
    ("paintOrder", string);
    ("panose1", string);
    ("pathLength", string);
    ("patternContentUnits", string);
    ("patternTransform", string);
    ("patternUnits", string);
    ("pointerEvents", string);
    ("points", string);
    ("pointsAtX", string);
    ("pointsAtY", string);
    ("pointsAtZ", string);
    ("preserveAlpha", string);
    ("preserveAspectRatio", string);
    ("primitiveUnits", string);
    ("r", string);
    ("radius", string);
    ("refX", string);
    ("refY", string);
    ("renderingIntent", string);
    ("repeatCount", string);
    ("repeatDur", string);
    ("requiredExtensions", string);
    ("requiredFeatures", string);
    ("restart", string);
    ("result", string);
    ("rotate", string);
    ("rx", string);
    ("ry", string);
    ("scale", string);
    ("seed", string);
    ("shapeRendering", string);
    ("slope", string);
    ("spacing", string);
    ("specularConstant", string);
    ("specularExponent", string);
    ("speed", string);
    ("spreadMethod", string);
    ("startOffset", string);
    ("stdDeviation", string);
    ("stemh", string);
    ("stemv", string);
    ("stitchTiles", string);
    ("stopColor", string);
    ("stopOpacity", string);
    ("strikethroughPosition", string);
    ("strikethroughThickness", string);
    (string, string);
    ("stroke", string);
    ("strokeDasharray", string);
    ("strokeDashoffset", string);
    ("strokeLinecap", string);
    ("strokeLinejoin", string);
    ("strokeMiterlimit", string);
    ("strokeOpacity", string);
    ("strokeWidth", string);
    ("surfaceScale", string);
    ("systemLanguage", string);
    ("tableValues", string);
    ("targetX", string);
    ("targetY", string);
    ("textAnchor", string);
    ("textDecoration", string);
    ("textLength", string);
    ("textRendering", string);
    ("transform", string);
    ("u1", string);
    ("u2", string);
    ("underlinePosition", string);
    ("underlineThickness", string);
    ("unicode", string);
    ("unicodeBidi", string);
    ("unicodeRange", string);
    ("unitsPerEm", string);
    ("vAlphabetic", string);
    ("vHanging", string);
    ("vIdeographic", string);
    ("vMathematical", string);
    ("values", string);
    ("vectorEffect", string);
    ("version", string);
    ("vertAdvX", string);
    ("vertAdvY", string);
    ("vertOriginX", string);
    ("vertOriginY", string);
    ("viewBox", string);
    ("viewTarget", string);
    ("visibility", string);
    ("widths", string);
    ("wordSpacing", string);
    ("writingMode", string);
    ("x", string);
    ("x1", string);
    ("x2", string);
    ("xChannelSelector", string);
    ("xHeight", string);
    ("xlinkActuate", string);
    ("xlinkArcrole", string);
    ("xlinkHref", string);
    ("xlinkRole", string);
    ("xlinkShow", string);
    ("xlinkTitle", string);
    ("xlinkType", string);
    ("xmlns", string);
    ("xmlnsXlink", string);
    ("xmlBase", string);
    ("xmlLang", string);
    ("xmlSpace", string);
    ("y", string);
    ("y1", string);
    ("y2", string);
    ("yChannelSelector", string);
    ("z", string);
    ("zoomAndPan", string);
    ("about", string);
    ("datatype", string);
    ("inlist", string);
    ("prefix", string);
    ("property", string);
    ("resource", string);
    ("typeof", string);
    ("vocab", string);
    ("dangerouslySetInnerHTML", "{\"__html\": string}");
    ("suppressContentEditableWarning", bool);
  ]

let showConstructor {Constructor.cname = {txt}; args; res} =
  txt
  ^ (match args with
    | [] -> ""
    | _ ->
      "("
      ^ (args
        |> List.map (fun (typ, _) -> typ |> Shared.typeToString)
        |> String.concat ", ")
      ^ ")")
  ^
  match res with
  | None -> ""
  | Some typ -> "\n" ^ (typ |> Shared.typeToString)

(* TODO: local opens *)
let resolveOpens ~env ~previous opens ~package =
  List.fold_left
    (fun previous path ->
      (* Finding an open, first trying to find it in previoulsly resolved opens *)
      let rec loop prev =
        match prev with
        | [] -> (
          match path with
          | [] | [_] -> previous
          | name :: path -> (
            match ProcessCmt.fileForModule ~package name with
            | None ->
              Log.log ("Could not get module " ^ name);
              previous (* TODO: warn? *)
            | Some file -> (
              match
                ResolvePath.resolvePath ~env:(QueryEnv.fromFile file) ~package
                  ~path
              with
              | None ->
                Log.log ("Could not resolve in " ^ name);
                previous
              | Some (env, _placeholder) -> previous @ [env])))
        | env :: rest -> (
          match ResolvePath.resolvePath ~env ~package ~path with
          | None -> loop rest
          | Some (env, _placeholder) -> previous @ [env])
      in
      Log.log ("resolving open " ^ pathToString path);
      match ResolvePath.resolvePath ~env ~package ~path with
      | None ->
        Log.log "Not local";
        loop previous
      | Some (env, _) ->
        Log.log "Was local";
        previous @ [env])
    (* loop(previous) *)
    previous opens

let checkName name ~prefix ~exact =
  if exact then name = prefix else Utils.startsWith name prefix

let completionForExporteds iterExported getDeclared ~prefix ~exact ~env
    ~namesUsed transformContents =
  let res = ref [] in
  iterExported (fun name stamp ->
      (* Log.log("checking exported: " ++ name); *)
      if checkName name ~prefix ~exact then
        match getDeclared stamp with
        | Some (declared : _ Declared.t)
          when not (Hashtbl.mem namesUsed declared.name.txt) ->
          Hashtbl.add namesUsed declared.name.txt ();
          res :=
            {
              (Completion.create ~name:declared.name.txt ~env
                 ~kind:(transformContents declared.item))
              with
              deprecated = declared.deprecated;
              docstring = declared.docstring;
            }
            :: !res
        | _ -> ());
  !res

let completionForExportedModules ~env ~prefix ~exact ~namesUsed =
  completionForExporteds (Exported.iter env.QueryEnv.exported Exported.Module)
    (Stamps.findModule env.file.stamps) ~prefix ~exact ~env ~namesUsed (fun m ->
      Completion.Module m)

let completionForExportedValues ~env ~prefix ~exact ~namesUsed =
  completionForExporteds (Exported.iter env.QueryEnv.exported Exported.Value)
    (Stamps.findValue env.file.stamps) ~prefix ~exact ~env ~namesUsed (fun v ->
      Completion.Value v)

let completionForExportedTypes ~env ~prefix ~exact ~namesUsed =
  completionForExporteds (Exported.iter env.QueryEnv.exported Exported.Type)
    (Stamps.findType env.file.stamps) ~prefix ~exact ~env ~namesUsed (fun t ->
      Completion.Type t)

let completionsForExportedConstructors ~(env : QueryEnv.t) ~prefix ~exact
    ~namesUsed =
  let res = ref [] in
  Exported.iter env.exported Exported.Type (fun _name stamp ->
      match Stamps.findType env.file.stamps stamp with
      | Some ({item = {kind = Type.Variant constructors}} as t) ->
        res :=
          (constructors
          |> List.filter (fun c ->
                 checkName c.Constructor.cname.txt ~prefix ~exact)
          |> Utils.filterMap (fun c ->
                 let name = c.Constructor.cname.txt in
                 if not (Hashtbl.mem namesUsed name) then
                   let () = Hashtbl.add namesUsed name () in
                   Some
                     (Completion.create ~name ~env
                        ~kind:
                          (Completion.Constructor
                             (c, t.item.decl |> Shared.declToString t.name.txt)))
                 else None))
          @ !res
      | _ -> ());
  !res

let completionForExportedFields ~(env : QueryEnv.t) ~prefix ~exact ~namesUsed =
  let res = ref [] in
  Exported.iter env.exported Exported.Type (fun _name stamp ->
      match Stamps.findType env.file.stamps stamp with
      | Some ({item = {kind = Record fields}} as t) ->
        res :=
          (fields
          |> List.filter (fun f -> checkName f.fname.txt ~prefix ~exact)
          |> Utils.filterMap (fun f ->
                 let name = f.fname.txt in
                 if not (Hashtbl.mem namesUsed name) then
                   let () = Hashtbl.add namesUsed name () in
                   Some
                     (Completion.create ~name ~env
                        ~kind:
                          (Completion.Field
                             (f, t.item.decl |> Shared.declToString t.name.txt)))
                 else None))
          @ !res
      | _ -> ());
  !res

let findModuleInScope ~env ~moduleName ~scope =
  let modulesTable = Hashtbl.create 10 in
  env.QueryEnv.file.stamps
  |> Stamps.iterModules (fun _ declared ->
         Hashtbl.replace modulesTable
           (declared.name.txt, declared.extentLoc |> Loc.start)
           declared);
  let result = ref None in
  let processModule name loc =
    if name = moduleName && !result = None then
      match Hashtbl.find_opt modulesTable (name, Loc.start loc) with
      | Some declared -> result := Some declared
      | None ->
        Log.log
          (Printf.sprintf "Module Not Found %s loc:%s\n" name (Loc.toString loc))
  in
  scope |> Scope.iterModulesBeforeFirstOpen processModule;
  scope |> Scope.iterModulesAfterFirstOpen processModule;
  !result

let resolvePathFromStamps ~(env : QueryEnv.t) ~package ~scope ~moduleName ~path
    =
  (* Log.log("Finding from stamps " ++ name); *)
  match findModuleInScope ~env ~moduleName ~scope with
  | None -> None
  | Some declared -> (
    (* Log.log("found it"); *)
    match ResolvePath.findInModule ~env declared.item path with
    | None -> None
    | Some res -> (
      match res with
      | `Local (env, name) -> Some (env, name)
      | `Global (moduleName, fullPath) -> (
        match ProcessCmt.fileForModule ~package moduleName with
        | None -> None
        | Some file ->
          ResolvePath.resolvePath ~env:(QueryEnv.fromFile file) ~path:fullPath
            ~package)))

let resolveModuleWithOpens ~opens ~package ~moduleName =
  let rec loop opens =
    match opens with
    | (env : QueryEnv.t) :: rest -> (
      Log.log ("Looking for env in " ^ Uri.toString env.file.uri);
      match ResolvePath.resolvePath ~env ~package ~path:[moduleName; ""] with
      | Some (env, _) -> Some env
      | None -> loop rest)
    | [] -> None
  in
  loop opens

let resolveFileModule ~moduleName ~package =
  Log.log ("Getting module " ^ moduleName);
  match ProcessCmt.fileForModule ~package moduleName with
  | None -> None
  | Some file ->
    Log.log "got it";
    let env = QueryEnv.fromFile file in
    Some env

let getEnvWithOpens ~scope ~(env : QueryEnv.t) ~package
    ~(opens : QueryEnv.t list) ~moduleName (path : string list) =
  (* TODO: handle interleaving of opens and local modules correctly *)
  match resolvePathFromStamps ~env ~scope ~moduleName ~path ~package with
  | Some x -> Some x
  | None -> (
    match resolveModuleWithOpens ~opens ~package ~moduleName with
    | Some env -> ResolvePath.resolvePath ~env ~package ~path
    | None -> (
      match resolveFileModule ~moduleName ~package with
      | None -> None
      | Some env -> ResolvePath.resolvePath ~env ~package ~path))

let detail name (kind : Completion.kind) =
  match kind with
  | Type {decl} -> decl |> Shared.declToString name
  | Value typ -> typ |> Shared.typeToString
  | ObjLabel typ -> typ |> Shared.typeToString
  | Label typString -> typString
  | Module _ -> "module"
  | FileModule _ -> "file module"
  | Field ({typ}, s) -> name ^ ": " ^ (typ |> Shared.typeToString) ^ "\n\n" ^ s
  | Constructor (c, s) -> showConstructor c ^ "\n\n" ^ s
  | PolyvariantConstructor {name; payload} -> (
    "#" ^ name
    ^
    match payload with
    | None -> ""
    | Some ({desc = Types.Ttuple _} as typeExpr) ->
      typeExpr |> Shared.typeToString
    | Some typeExpr -> "(" ^ (typeExpr |> Shared.typeToString) ^ ")")

let findAllCompletions ~(env : QueryEnv.t) ~prefix ~exact ~namesUsed
    ~(completionContext : Completable.completionContext) =
  Log.log ("findAllCompletions uri:" ^ Uri.toString env.file.uri);
  match completionContext with
  | Value ->
    completionForExportedValues ~env ~prefix ~exact ~namesUsed
    @ completionsForExportedConstructors ~env ~prefix ~exact ~namesUsed
    @ completionForExportedModules ~env ~prefix ~exact ~namesUsed
  | Type ->
    completionForExportedTypes ~env ~prefix ~exact ~namesUsed
    @ completionForExportedModules ~env ~prefix ~exact ~namesUsed
  | Module -> completionForExportedModules ~env ~prefix ~exact ~namesUsed
  | Field ->
    completionForExportedFields ~env ~prefix ~exact ~namesUsed
    @ completionForExportedModules ~env ~prefix ~exact ~namesUsed

module LocalTables = struct
  type 'a table = (string * (int * int), 'a Declared.t) Hashtbl.t
  type namesUsed = (string, unit) Hashtbl.t

  type t = {
    namesUsed: namesUsed;
    mutable resultRev: Completion.t list;
    constructorTable: Constructor.t table;
    modulesTable: Module.t table;
    typesTable: Type.t table;
    valueTable: Types.type_expr table;
  }

  let create () =
    {
      namesUsed = Hashtbl.create 1;
      resultRev = [];
      constructorTable = Hashtbl.create 1;
      modulesTable = Hashtbl.create 1;
      typesTable = Hashtbl.create 1;
      valueTable = Hashtbl.create 1;
    }

  let populateValues ~env localTables =
    env.QueryEnv.file.stamps
    |> Stamps.iterValues (fun _ declared ->
           Hashtbl.replace localTables.valueTable
             (declared.name.txt, declared.extentLoc |> Loc.start)
             declared)

  let populateConstructors ~env localTables =
    env.QueryEnv.file.stamps
    |> Stamps.iterConstructors (fun _ declared ->
           Hashtbl.replace localTables.constructorTable
             (declared.name.txt, declared.extentLoc |> Loc.start)
             declared)

  let populateTypes ~env localTables =
    env.QueryEnv.file.stamps
    |> Stamps.iterTypes (fun _ declared ->
           Hashtbl.replace localTables.typesTable
             (declared.name.txt, declared.name.loc |> Loc.start)
             declared)

  let populateModules ~env localTables =
    env.QueryEnv.file.stamps
    |> Stamps.iterModules (fun _ declared ->
           Hashtbl.replace localTables.modulesTable
             (declared.name.txt, declared.extentLoc |> Loc.start)
             declared)
end

let processLocalValue name loc ~prefix ~exact ~env
    ~(localTables : LocalTables.t) =
  if checkName name ~prefix ~exact then
    match Hashtbl.find_opt localTables.valueTable (name, Loc.start loc) with
    | Some declared ->
      if not (Hashtbl.mem localTables.namesUsed name) then (
        Hashtbl.add localTables.namesUsed name ();
        localTables.resultRev <-
          {
            (Completion.create ~name:declared.name.txt ~env
               ~kind:(Value declared.item))
            with
            deprecated = declared.deprecated;
            docstring = declared.docstring;
          }
          :: localTables.resultRev)
    | None ->
      Log.log
        (Printf.sprintf "Completion Value Not Found %s loc:%s\n" name
           (Loc.toString loc));
      localTables.resultRev <-
        Completion.create ~name ~env
          ~kind:
            (Value
               (Ctype.newconstr
                  (Path.Pident (Ident.create "Type Not Known"))
                  []))
        :: localTables.resultRev

let processLocalConstructor name loc ~prefix ~exact ~env
    ~(localTables : LocalTables.t) =
  if checkName name ~prefix ~exact then
    match
      Hashtbl.find_opt localTables.constructorTable (name, Loc.start loc)
    with
    | Some declared ->
      if not (Hashtbl.mem localTables.namesUsed name) then (
        Hashtbl.add localTables.namesUsed name ();
        localTables.resultRev <-
          {
            (Completion.create ~name:declared.name.txt ~env
               ~kind:
                 (Constructor
                    ( declared.item,
                      snd declared.item.typeDecl
                      |> Shared.declToString (fst declared.item.typeDecl) )))
            with
            deprecated = declared.deprecated;
            docstring = declared.docstring;
          }
          :: localTables.resultRev)
    | None ->
      Log.log
        (Printf.sprintf "Completion Constructor Not Found %s loc:%s\n" name
           (Loc.toString loc))

let processLocalType name loc ~prefix ~exact ~env ~(localTables : LocalTables.t)
    =
  if checkName name ~prefix ~exact then
    match Hashtbl.find_opt localTables.typesTable (name, Loc.start loc) with
    | Some declared ->
      if not (Hashtbl.mem localTables.namesUsed name) then (
        Hashtbl.add localTables.namesUsed name ();
        localTables.resultRev <-
          {
            (Completion.create ~name:declared.name.txt ~env
               ~kind:(Type declared.item))
            with
            deprecated = declared.deprecated;
            docstring = declared.docstring;
          }
          :: localTables.resultRev)
    | None ->
      Log.log
        (Printf.sprintf "Completion Type Not Found %s loc:%s\n" name
           (Loc.toString loc))

let processLocalModule name loc ~prefix ~exact ~env
    ~(localTables : LocalTables.t) =
  if checkName name ~prefix ~exact then
    match Hashtbl.find_opt localTables.modulesTable (name, Loc.start loc) with
    | Some declared ->
      if not (Hashtbl.mem localTables.namesUsed name) then (
        Hashtbl.add localTables.namesUsed name ();
        localTables.resultRev <-
          {
            (Completion.create ~name:declared.name.txt ~env
               ~kind:(Module declared.item))
            with
            deprecated = declared.deprecated;
            docstring = declared.docstring;
          }
          :: localTables.resultRev)
    | None ->
      Log.log
        (Printf.sprintf "Completion Module Not Found %s loc:%s\n" name
           (Loc.toString loc))

let getItemsFromOpens ~opens ~localTables ~prefix ~exact ~completionContext =
  opens
  |> List.fold_left
       (fun results env ->
         let completionsFromThisOpen =
           findAllCompletions ~env ~prefix ~exact
             ~namesUsed:localTables.LocalTables.namesUsed ~completionContext
         in
         completionsFromThisOpen @ results)
       []

(* This is a dedicated function for finding local completions for a specific type expr (as in filtered to match only the type expr).
   Filtering is commented out for now though, since the strategy I used did not work out.
   The idea here is that _values_ can be filtered on the type, but we still need to complete for modules just like we normally do,
   to support using values from other modules. *)
let findLocalCompletionsForTypeExpr ~(localTables : LocalTables.t) ~env ~prefix
    ~exact ~opens ~scope typeExpr =
  let _targetId = typeExpr.Types.id in
  localTables |> LocalTables.populateValues ~env;
  localTables |> LocalTables.populateModules ~env;
  scope
  |> Scope.iterValuesBeforeFirstOpen
       (processLocalValue ~prefix ~exact ~env ~localTables);
  scope
  |> Scope.iterModulesBeforeFirstOpen
       (processLocalModule ~prefix ~exact ~env ~localTables);

  let valuesFromOpens =
    getItemsFromOpens ~opens ~localTables ~prefix ~exact
      ~completionContext:Value
  in

  scope
  |> Scope.iterValuesAfterFirstOpen
       (processLocalValue ~prefix ~exact ~env ~localTables);
  scope
  |> Scope.iterModulesAfterFirstOpen
       (processLocalModule ~prefix ~exact ~env ~localTables);
  List.rev_append localTables.resultRev valuesFromOpens
(* TODO: Re-enable/make this work - filtering by type |> List.filter (fun completion ->
       match completion.Completion.kind with
       | Module _ -> true
       | Value {id} when id = targetId -> true
       | Value {id} ->
         Printf.printf "compared %s with id %i to target %i\n" completion.name
           id targetId;
         false
       | _ -> false) *)

let findLocalCompletionsForValuesAndConstructors ~(localTables : LocalTables.t)
    ~env ~prefix ~exact ~opens ~scope =
  localTables |> LocalTables.populateValues ~env;
  localTables |> LocalTables.populateConstructors ~env;
  localTables |> LocalTables.populateModules ~env;
  scope
  |> Scope.iterValuesBeforeFirstOpen
       (processLocalValue ~prefix ~exact ~env ~localTables);
  scope
  |> Scope.iterConstructorsBeforeFirstOpen
       (processLocalConstructor ~prefix ~exact ~env ~localTables);
  scope
  |> Scope.iterModulesBeforeFirstOpen
       (processLocalModule ~prefix ~exact ~env ~localTables);

  let valuesFromOpens =
    getItemsFromOpens ~opens ~localTables ~prefix ~exact
      ~completionContext:Value
  in

  scope
  |> Scope.iterValuesAfterFirstOpen
       (processLocalValue ~prefix ~exact ~env ~localTables);
  scope
  |> Scope.iterConstructorsAfterFirstOpen
       (processLocalConstructor ~prefix ~exact ~env ~localTables);
  scope
  |> Scope.iterModulesAfterFirstOpen
       (processLocalModule ~prefix ~exact ~env ~localTables);
  List.rev_append localTables.resultRev valuesFromOpens

let findLocalCompletionsForTypes ~(localTables : LocalTables.t) ~env ~prefix
    ~exact ~opens ~scope =
  localTables |> LocalTables.populateTypes ~env;
  localTables |> LocalTables.populateModules ~env;
  scope
  |> Scope.iterTypesBeforeFirstOpen
       (processLocalType ~prefix ~exact ~env ~localTables);
  scope
  |> Scope.iterModulesBeforeFirstOpen
       (processLocalModule ~prefix ~exact ~env ~localTables);

  let valuesFromOpens =
    getItemsFromOpens ~opens ~localTables ~prefix ~exact ~completionContext:Type
  in

  scope
  |> Scope.iterTypesAfterFirstOpen
       (processLocalType ~prefix ~exact ~env ~localTables);
  scope
  |> Scope.iterModulesAfterFirstOpen
       (processLocalModule ~prefix ~exact ~env ~localTables);
  List.rev_append localTables.resultRev valuesFromOpens

let findLocalCompletionsForModules ~(localTables : LocalTables.t) ~env ~prefix
    ~exact ~opens ~scope =
  localTables |> LocalTables.populateModules ~env;
  scope
  |> Scope.iterModulesBeforeFirstOpen
       (processLocalModule ~prefix ~exact ~env ~localTables);

  let valuesFromOpens =
    getItemsFromOpens ~opens ~localTables ~prefix ~exact
      ~completionContext:Module
  in

  scope
  |> Scope.iterModulesAfterFirstOpen
       (processLocalModule ~prefix ~exact ~env ~localTables);
  List.rev_append localTables.resultRev valuesFromOpens

let findLocalCompletionsWithOpens ~pos ~(env : QueryEnv.t) ~prefix ~exact ~opens
    ~scope ~(completionContext : Completable.completionContext) =
  (* TODO: handle arbitrary interleaving of opens and local bindings correctly *)
  Log.log
    ("findLocalCompletionsWithOpens uri:" ^ Uri.toString env.file.uri ^ " pos:"
   ^ Pos.toString pos);
  let localTables = LocalTables.create () in
  match completionContext with
  | Value ->
    findLocalCompletionsForValuesAndConstructors ~localTables ~env ~prefix
      ~exact ~opens ~scope
  | Type ->
    findLocalCompletionsForTypes ~localTables ~env ~prefix ~exact ~opens ~scope
  | Module ->
    findLocalCompletionsForModules ~localTables ~env ~prefix ~exact ~opens
      ~scope
  | Field ->
    (* There's no local completion for fields *)
    []

let findLocalCompletionsForTypeExprWithOpens ~(env : QueryEnv.t) ~prefix ~exact
    ~opens ~scope typeExpr =
  (* TODO: handle arbitrary interleaving of opens and local bindings correctly *)
  let localTables = LocalTables.create () in
  typeExpr
  |> findLocalCompletionsForTypeExpr ~localTables ~env ~exact ~opens ~scope
       ~prefix

let rec extractRecordType ~env ~package (t : Types.type_expr) =
  match t.desc with
  | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> extractRecordType ~env ~package t1
  | Tconstr (path, _, _) -> (
    match References.digConstructor ~env ~package path with
    | Some (env, ({item = {kind = Record fields}} as typ)) ->
      Some (env, fields, typ)
    | Some (env, {item = {decl = {type_manifest = Some t1}}}) ->
      extractRecordType ~env ~package t1
    | _ -> None)
  | _ -> None

(* Had to add another type level here to cover that polyvariants are inlined into to the type system. Mostly a temporary solution. *)
type extractedType =
  | Declared of QueryEnv.t * Type.t Declared.t
  | Polyvariant of QueryEnv.t * SharedTypes.polyVariantConstructor list
  | Tuple of QueryEnv.t * Types.type_expr list
  | Toption of QueryEnv.t * Types.type_expr

(* This is a more general extraction function for pulling out the type of a type_expr. We already have other similar functions, but they are all specialized on something (variants, records, etc). *)
let rec extractType ~env ~package (t : Types.type_expr) =
  match t.desc with
  | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> extractType ~env ~package t1
  | Tconstr (Path.Pident {name = "option"}, [payloadTypeExpr], _) ->
    (* Handle option. TODO: Look up how the compiler does this and copy that behavior. *)
    Some (Toption (env, payloadTypeExpr))
  | Tconstr (path, _, _) -> (
    match References.digConstructor ~env ~package path with
    | Some (env, {item = {decl = {type_manifest = Some t1}}}) ->
      extractType ~env ~package t1
    | Some (env, typ) -> Some (Declared (env, typ))
    | None -> None)
  | Tvariant {row_fields} ->
    (* Since polyvariants are strutural, they're "inlined". So, we extract just
       what we need for completion from that definition here. *)
    let constructors =
      row_fields
      |> List.map (fun (label, field) ->
             {
               name = label;
               payload =
                 (match field with
                 | Types.Rpresent maybeTypeExpr -> maybeTypeExpr
                 | _ -> None);
               args =
                 (* Multiple arguments are represented as a Ttuple, while a single argument is just the type expression itself. *)
                 (match field with
                 | Types.Rpresent (Some typeExpr) -> (
                   match typeExpr.desc with
                   | Ttuple args -> args
                   | _ -> [typeExpr])
                 | _ -> []);
             })
    in
    Some (Polyvariant (env, constructors))
  | Ttuple expressions -> Some (Tuple (env, expressions))
  | _ -> None

let rec extractObjectType ~env ~package (t : Types.type_expr) =
  match t.desc with
  | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> extractObjectType ~env ~package t1
  | Tobject (tObj, _) -> Some (env, tObj)
  | Tconstr (path, _, _) -> (
    match References.digConstructor ~env ~package path with
    | Some (env, {item = {decl = {type_manifest = Some t1}}}) ->
      extractObjectType ~env ~package t1
    | _ -> None)
  | _ -> None

let extractFunctionType ~env ~package typ =
  let rec loop ~env acc (t : Types.type_expr) =
    match t.desc with
    | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> loop ~env acc t1
    | Tarrow (label, tArg, tRet, _) -> loop ~env ((label, tArg) :: acc) tRet
    | Tconstr (path, _, _) -> (
      match References.digConstructor ~env ~package path with
      | Some (env, {item = {decl = {type_manifest = Some t1}}}) ->
        loop ~env acc t1
      | _ -> (List.rev acc, t))
    | _ -> (List.rev acc, t)
  in
  loop ~env [] typ

let getCompletionsForPath ~package ~opens ~allFiles ~pos ~exact ~scope
    ~completionContext ~env path =
  match path with
  | [] -> []
  | [prefix] ->
    let localCompletionsWithOpens =
      findLocalCompletionsWithOpens ~pos ~env ~prefix ~exact ~opens ~scope
        ~completionContext
    in
    let fileModules =
      allFiles |> FileSet.elements
      |> Utils.filterMap (fun name ->
             if
               checkName name ~prefix ~exact
               && not
                    (* TODO complete the namespaced name too *)
                    (String.contains name '-')
             then
               Some
                 (Completion.create ~name ~env
                    ~kind:(Completion.FileModule name))
             else None)
    in
    localCompletionsWithOpens @ fileModules
  | moduleName :: path -> (
    Log.log ("Path " ^ pathToString path);
    match getEnvWithOpens ~scope ~env ~package ~opens ~moduleName path with
    | Some (env, prefix) ->
      Log.log "Got the env";
      let namesUsed = Hashtbl.create 10 in
      findAllCompletions ~env ~prefix ~exact ~namesUsed ~completionContext
    | None -> [])

let mkItem ~name ~kind ~detail ~deprecated ~docstring =
  let docContent =
    (match deprecated with
    | None -> ""
    | Some s -> "Deprecated: " ^ s ^ "\n\n")
    ^
    match docstring with
    | [] -> ""
    | _ :: _ -> docstring |> String.concat "\n"
  in
  let tags =
    match deprecated with
    | None -> []
    | Some _ -> [1 (* deprecated *)]
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

let completionToItem {Completion.name; deprecated; docstring; kind} =
  mkItem ~name
    ~kind:(Completion.kindToInt kind)
    ~deprecated ~detail:(detail name kind) ~docstring

let completionsGetTypeEnv = function
  | {Completion.kind = Value typ; env} :: _ -> Some (typ, env)
  | {Completion.kind = ObjLabel typ; env} :: _ -> Some (typ, env)
  | {Completion.kind = Field ({typ}, _); env} :: _ -> Some (typ, env)
  | _ -> None

let rec getCompletionsForContextPath ~package ~opens ~rawOpens ~allFiles ~pos
    ~env ~exact ~scope (contextPath : Completable.contextPath) =
  match contextPath with
  | CPString ->
    [
      Completion.create ~name:"string" ~env
        ~kind:
          (Completion.Value
             (Ctype.newconstr (Path.Pident (Ident.create "string")) []));
    ]
  | CPArray ->
    [
      Completion.create ~name:"array" ~env
        ~kind:
          (Completion.Value
             (Ctype.newconstr (Path.Pident (Ident.create "array")) []));
    ]
  | CPId (path, completionContext) ->
    path
    |> getCompletionsForPath ~package ~opens ~allFiles ~pos ~exact
         ~completionContext ~env ~scope
  | CPApply (cp, labels) -> (
    match
      cp
      |> getCompletionsForContextPath ~package ~opens ~rawOpens ~allFiles ~pos
           ~env ~exact:true ~scope
      |> completionsGetTypeEnv
    with
    | Some (typ, env) -> (
      let rec reconstructFunctionType args tRet =
        match args with
        | [] -> tRet
        | (label, tArg) :: rest ->
          let restType = reconstructFunctionType rest tRet in
          {typ with desc = Tarrow (label, tArg, restType, Cok)}
      in
      let rec processApply args labels =
        match (args, labels) with
        | _, [] -> args
        | _, label :: (_ :: _ as nextLabels) ->
          (* compute the application of the first label, then the next ones *)
          let args = processApply args [label] in
          processApply args nextLabels
        | (Asttypes.Nolabel, _) :: nextArgs, [Asttypes.Nolabel] -> nextArgs
        | ((Labelled _, _) as arg) :: nextArgs, [Nolabel] ->
          arg :: processApply nextArgs labels
        | (Optional _, _) :: nextArgs, [Nolabel] -> processApply nextArgs labels
        | ( (((Labelled s1 | Optional s1), _) as arg) :: nextArgs,
            [(Labelled s2 | Optional s2)] ) ->
          if s1 = s2 then nextArgs else arg :: processApply nextArgs labels
        | ((Nolabel, _) as arg) :: nextArgs, [(Labelled _ | Optional _)] ->
          arg :: processApply nextArgs labels
        | [], [(Nolabel | Labelled _ | Optional _)] ->
          (* should not happen, but just ignore extra arguments *) []
      in
      match extractFunctionType ~env ~package typ with
      | args, tRet when args <> [] ->
        let args = processApply args labels in
        let retType = reconstructFunctionType args tRet in
        [Completion.create ~name:"dummy" ~env ~kind:(Completion.Value retType)]
      | _ -> [])
    | None -> [])
  | CPField (CPId (path, Module), fieldName) ->
    (* M.field *)
    path @ [fieldName]
    |> getCompletionsForPath ~package ~opens ~allFiles ~pos ~exact
         ~completionContext:Field ~env ~scope
  | CPField (cp, fieldName) -> (
    match
      cp
      |> getCompletionsForContextPath ~package ~opens ~rawOpens ~allFiles ~pos
           ~env ~exact:true ~scope
      |> completionsGetTypeEnv
    with
    | Some (typ, env) -> (
      match typ |> extractRecordType ~env ~package with
      | Some (env, fields, typDecl) ->
        fields
        |> Utils.filterMap (fun field ->
               if checkName field.fname.txt ~prefix:fieldName ~exact then
                 Some
                   (Completion.create ~name:field.fname.txt ~env
                      ~kind:
                        (Completion.Field
                           ( field,
                             typDecl.item.decl
                             |> Shared.declToString typDecl.name.txt )))
               else None)
      | None -> [])
    | None -> [])
  | CPObj (cp, label) -> (
    match
      cp
      |> getCompletionsForContextPath ~package ~opens ~rawOpens ~allFiles ~pos
           ~env ~exact:true ~scope
      |> completionsGetTypeEnv
    with
    | Some (typ, env) -> (
      match typ |> extractObjectType ~env ~package with
      | Some (env, tObj) ->
        let rec getFields (texp : Types.type_expr) =
          match texp.desc with
          | Tfield (name, _, t1, t2) ->
            let fields = t2 |> getFields in
            (name, t1) :: fields
          | Tlink te | Tsubst te | Tpoly (te, []) -> te |> getFields
          | Tvar None -> []
          | _ -> []
        in
        tObj |> getFields
        |> Utils.filterMap (fun (field, typ) ->
               if checkName field ~prefix:label ~exact then
                 Some
                   (Completion.create ~name:field ~env
                      ~kind:(Completion.ObjLabel typ))
               else None)
      | None -> [])
    | None -> [])
  | CPPipe (cp, funNamePrefix) -> (
    match
      cp
      |> getCompletionsForContextPath ~package ~opens ~rawOpens ~allFiles ~pos
           ~env ~exact:true ~scope
      |> completionsGetTypeEnv
    with
    | Some (typ, _envNotUsed) -> (
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
        | _ -> (
          match loop path with
          | _ :: rest -> List.rev rest
          | [] -> [])
      in
      let getConstr typ =
        match typ.Types.desc with
        | Tconstr (path, _, _)
        | Tlink {desc = Tconstr (path, _, _)}
        | Tsubst {desc = Tconstr (path, _, _)}
        | Tpoly ({desc = Tconstr (path, _, _)}, []) ->
          Some path
        | _ -> None
      in
      let fromType typ =
        match getConstr typ with
        | None -> None
        | Some path -> Some (getModulePath path)
      in
      let lhsPath = fromType typ in
      let removePackageOpens modulePath =
        match modulePath with
        | toplevel :: rest when package.opens |> List.mem toplevel -> rest
        | _ -> modulePath
      in
      let rec removeRawOpen rawOpen modulePath =
        match (rawOpen, modulePath) with
        | [_], _ -> Some modulePath
        | s :: inner, first :: restPath when s = first ->
          removeRawOpen inner restPath
        | _ -> None
      in
      let rec removeRawOpens rawOpens modulePath =
        match rawOpens with
        | rawOpen :: restOpens -> (
          let newModulePath = removeRawOpens restOpens modulePath in
          match removeRawOpen rawOpen newModulePath with
          | None -> newModulePath
          | Some mp -> mp)
        | [] -> modulePath
      in
      match lhsPath with
      | Some modulePath -> (
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
          let completions =
            modulePath @ [funNamePrefix]
            |> getCompletionsForPath ~completionContext:Value ~exact:false
                 ~package ~opens ~allFiles ~pos ~env ~scope
          in
          completions
          |> List.map (fun (completion : Completion.t) ->
                 {
                   completion with
                   name = completionName completion.name;
                   env
                   (* Restore original env for the completion after x->foo()... *);
                 })
        | [] -> [])
      | None -> [])
    | None -> [])

let getOpens ~rawOpens ~package ~env =
  Log.log
    ("Raw ppens: "
    ^ string_of_int (List.length rawOpens)
    ^ " "
    ^ String.concat " ... " (rawOpens |> List.map pathToString));
  let packageOpens = package.opens in
  Log.log ("Package opens " ^ String.concat " " packageOpens);
  let resolvedOpens =
    resolveOpens ~env
      ~previous:
        (List.map QueryEnv.fromFile
           (packageOpens |> Utils.filterMap (ProcessCmt.fileForModule ~package)))
      (List.rev rawOpens) ~package
  in
  Log.log
    ("Resolved opens "
    ^ string_of_int (List.length resolvedOpens)
    ^ " "
    ^ String.concat " "
        (resolvedOpens
        |> List.map (fun (e : QueryEnv.t) -> Uri.toString e.file.uri)));
  (* Last open takes priority *)
  List.rev resolvedOpens

(* Temporary while I figure things out. This should be merged with the completable we already have. *)
type completable =
  | CRecord of {
      fields: field list;
      decl: Types.type_declaration;
      name: string Location.loc;
    }
  | CVariant of {constructors: Constructor.t list}
  | CPolyVariant of {constructors: polyVariantConstructor list}

let typeExprToCompletable typ ~env ~package =
  match typ |> extractType ~env ~package with
  | Some (Declared (_, {name; item = {kind = Record fields; decl}})) ->
    Some (CRecord {fields; decl; name})
  | Some (Declared (_, {item = {kind = Variant constructors}})) ->
    Some (CVariant {constructors})
  | Some (Polyvariant (_, constructors)) -> Some (CPolyVariant {constructors})
  | _ -> None

(* Takes a type_expr and figures out what completable it corresponds to, if any. If the target type is nested somewhere inside of the type_expr,
   nestedContextPath will be populated with instructions on how to reach that part. This is primarily used in patterns,
   where the cursor can be deep into the pattern. *)
let findTypeInContext (typ : Types.type_expr) ~env ~nestedContextPath ~package =
  let rec digToType (currentType : Types.type_expr) ~env ~nestedContextPath
      ~package =
    (* Check the current context path item *)
    match nestedContextPath with
    | [] ->
      (* If there's no context path, complete the current type *)
      typeExprToCompletable currentType ~env ~package
    | targetItem :: nestedContextPath -> (
      match (targetItem, currentType |> extractType ~env ~package) with
      | ( Completable.RField {fieldName},
          Some (Declared (_, {item = {kind = Record fields}})) ) -> (
        match
          fields |> List.find_opt (fun field -> field.fname.txt = fieldName)
        with
        | None -> None
        | Some targetField ->
          if List.length nestedContextPath > 0 then
            targetField.typ |> digToType ~env ~nestedContextPath ~package
          else typeExprToCompletable targetField.typ ~env ~package)
      | ( Variant {constructorName; payloadNum},
          Some (Declared (_, {item = {kind = Variant constructors}})) ) -> (
        match
          constructors
          |> List.find_opt (fun constructor ->
                 constructor.Constructor.cname.txt = constructorName)
        with
        | Some constructor -> (
          (* payloadNum tells us whether there's also a payload we should descend into. *)
          match payloadNum with
          | None -> Some (CVariant {constructors})
          | Some argNum -> (
            (* If we find the argument/payload requested, descend into that *)
            match List.nth_opt constructor.args argNum with
            | None -> None
            | Some (argType, _) ->
              argType |> digToType ~env ~nestedContextPath ~package))
        | None -> None)
      | Polyvariant {name; payloadNum}, Some (Polyvariant (_, constructors))
        -> (
        match
          constructors
          |> List.find_opt (fun constructor -> constructor.name = name)
        with
        | Some constructor -> (
          match payloadNum with
          | None -> Some (CPolyVariant {constructors})
          | Some argNum -> (
            match List.nth_opt constructor.args argNum with
            | None -> None
            | Some argType ->
              argType |> digToType ~env ~nestedContextPath ~package))
        | None -> None)
      | PTuple {itemNumber}, Some (Tuple (_, exprs)) -> (
        match List.nth_opt exprs itemNumber with
        | None -> None
        | Some typeExpr ->
          typeExpr |> digToType ~env ~nestedContextPath ~package)
      | Variant {constructorName = "Some"}, Some (Toption (_, typeExpr)) ->
        typeExpr |> digToType ~env ~nestedContextPath ~package
      | _ -> None)
  in
  typ |> digToType ~env ~nestedContextPath ~package

(* Takes a context and a component path (SomeModule.SomeComponent) and extracts all labels *)
let getLabelsForComponent ~package ~opens ~allFiles ~pos ~env ~scope
    componentPath =
  let findTypeOfValue path =
    path
    |> getCompletionsForPath ~completionContext:Value ~exact:true ~package
         ~opens ~allFiles ~pos ~env ~scope
    |> completionsGetTypeEnv
  in

  match componentPath @ ["make"] |> findTypeOfValue with
  | Some (typ, _env) ->
    let rec getFields (texp : Types.type_expr) =
      match texp.desc with
      | Tfield (name, _, t1, t2) ->
        let fields = t2 |> getFields in
        if name = "children" then fields else (name, t1) :: fields
      | Tlink te | Tsubst te | Tpoly (te, []) -> te |> getFields
      | Tvar None -> []
      | _ -> []
    in
    let rec getLabels (t : Types.type_expr) =
      match t.desc with
      | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> getLabels t1
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
  | None -> []

(* This pulls out the type we want to do type based completion based on, using an instruction.
   This instruction can be "the type that the jsx prop X wants in component Y". *)
let findSourceType sourceType ~package ~opens ~rawOpens ~allFiles ~env ~scope
    ~pos =
  match sourceType with
  | Completable.NamedArg {label; contextPath} -> (
    (* TODO: Should probably share this with the branch handling CnamedArg... *)
    let labels =
      match
        (* This just hijacks getCompletionsForContextPath and completionsGetTypeEnv because they
           already kind of do what I need. Should break this out to its own, dedicated thing later. *)
        contextPath
        |> getCompletionsForContextPath ~package ~opens ~rawOpens ~allFiles ~pos
             ~env ~exact:true ~scope
        |> completionsGetTypeEnv
      with
      | Some (typ, env) ->
        let rec getLabels ~env (t : Types.type_expr) =
          match t.desc with
          | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> getLabels ~env t1
          | Tarrow ((Labelled l | Optional l), tArg, tRet, _) ->
            (l, tArg) :: getLabels ~env tRet
          | Tarrow (Nolabel, _, tRet, _) -> getLabels ~env tRet
          | Tconstr (path, _, _) -> (
            match References.digConstructor ~env ~package path with
            | Some (env, {item = {decl = {type_manifest = Some t1}}}) ->
              getLabels ~env t1
            | _ -> [])
          | _ -> []
        in
        typ |> getLabels ~env
      | None -> []
    in
    let targetLabel =
      labels |> List.find_opt (fun (name, _t) -> name = label)
    in
    match targetLabel with
    | Some (_, typeExpr) -> Some typeExpr
    | _ -> None)
  | JsxProp {componentPath; propName} -> (
    match
      componentPath
      |> getLabelsForComponent ~package ~opens ~allFiles ~pos ~env ~scope
      |> List.find_opt (fun (label, _typeExpr) -> label = propName)
    with
    | None -> None
    | Some (_label, typeExpr) -> Some typeExpr)
  | CtxPath typeSourceContextPath -> (
    match
      typeSourceContextPath
      |> getCompletionsForContextPath ~package ~opens ~rawOpens ~allFiles ~pos
           ~env ~exact:true ~scope
      |> completionsGetTypeEnv
    with
    | Some (typ, _env) -> Some typ
    | None -> None)

let processCompletable ~debug ~package ~scope ~env ~pos ~forHover
    (completable : Completable.t) =
  let rawOpens = Scope.getRawOpens scope in
  let opens = getOpens ~rawOpens ~package ~env in
  let allFiles = FileSet.union package.projectFiles package.dependenciesFiles in
  match completable with
  | Cnone -> []
  | Cpath contextPath ->
    contextPath
    |> getCompletionsForContextPath ~package ~opens ~rawOpens ~allFiles ~pos
         ~env ~exact:forHover ~scope
  | Cjsx ([id], prefix, identsSeen) when String.uncapitalize_ascii id = id ->
    let mkLabel (name, typString) =
      Completion.create ~name ~kind:(Label typString) ~env
    in
    let keyLabels =
      if Utils.startsWith "key" prefix then [mkLabel ("key", "string")] else []
    in
    (domLabels
    |> List.filter (fun (name, _t) ->
           Utils.startsWith name prefix
           && (forHover || not (List.mem name identsSeen)))
    |> List.map mkLabel)
    @ keyLabels
  | Cjsx (componentPath, prefix, identsSeen) ->
    let labels =
      componentPath
      |> getLabelsForComponent ~package ~opens ~allFiles ~pos ~env ~scope
    in
    let mkLabel_ name typString =
      Completion.create ~name ~kind:(Label typString) ~env
    in
    let mkLabel (name, typ) = mkLabel_ name (typ |> Shared.typeToString) in
    let keyLabels =
      if Utils.startsWith "key" prefix then [mkLabel_ "key" "string"] else []
    in
    if labels = [] then []
    else
      (labels
      |> List.filter (fun (name, _t) ->
             Utils.startsWith name prefix
             && (forHover || not (List.mem name identsSeen)))
      |> List.map mkLabel)
      @ keyLabels
  | Cdecorator prefix ->
    let mkDecorator (name, docstring) =
      {(Completion.create ~name ~kind:(Label "") ~env) with docstring}
    in
    [
      ( "as",
        [
          {|The `@as` decorator is commonly used on record types to alias record field names to a different JavaScript attribute name.

This is useful to map to JavaScript attribute names that cannot be expressed in ReScript (such as keywords).

It is also possible to map a ReScript record to a JavaScript array by passing indices to the `@as` decorator.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#as-decorator).|};
        ] );
      ( "dead",
        [
          {|The `@dead` decorator is for reanalyze, a static analysis tool for ReScript that can do dead code analysis.

`@dead` suppresses reporting on the value/type, but can also be used to force the analysis to consider a value as dead. Typically used to acknowledge cases of dead code you are not planning to address right now, but can be searched easily later.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#dead-decorator).

> Hint: Did you know you can run an interactive code analysis in your project by running the command `> ReScript: Start Code Analyzer`? Try it!|};
        ] );
      ( "deriving",
        [
          {|When the `@deriving` decorator is applied to a record type, it expands the type into a factory function plus a set of getter/setter functions for its fields.
      
[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#deriving-decorator).|};
        ] );
      ( "deprecated",
        [
          {|The `@deprecated` decorator is used to add deprecation notes to types, values and submodules. The compiler and editor tooling will yield a warning whenever a deprecated entity is being used.

Alternatively, use the `@@deprecated` decorator to add a deprecation warning to the file level.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#expression-deprecated-decorator).|};
        ] );
      ( "doesNotRaise",
        [
          {|The `@doesNotRaise` decorator is for reanalyze, a static analysis tool for ReScript that can perform exception analysis.

`@doesNotRaise` is uses to override the analysis and state that an expression does not raise any exceptions,
even though the analysis reports otherwise. This can happen for example in the case of array access where
the analysis does not perform range checks but takes a conservative stance that any access
could potentially raise.
[Read more and see examples in the documentation](https://github.com/rescript-association/reanalyze/blob/master/EXCEPTION.md).
> Hint: Did you know you can run an interactive code analysis in your project by running the command `> ReScript: Start Code Analyzer`? Try it!|};
        ] );
      ( "genType",
        [
          {|The @genType decorator may be used to export ReScript values and types to JavaScript, and import JavaScript values and types into ReScript. It allows seamless integration of compiled ReScript modules in existing TypeScript, Flow, or plain JavaScript codebases, without loosing type information across different type systems.
      
[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#gentype-decorator).|};
        ] );
      ( "genType.as",
        [
          {|The @genType decorator may be used to export ReScript values and types to JavaScript, and import JavaScript values and types into ReScript. It allows seamless integration of compiled ReScript modules in existing TypeScript, Flow, or plain JavaScript codebases, without loosing type information across different type systems.
    
[Read more and see examples in the documentation](https://rescript-lang.org/docs/gentype/latest/usage).|};
        ] );
      ( "genType.import",
        [
          {|The @genType decorator may be used to export ReScript values and types to JavaScript, and import JavaScript values and types into ReScript. It allows seamless integration of compiled ReScript modules in existing TypeScript, Flow, or plain JavaScript codebases, without loosing type information across different type systems.
    
[Read more and see examples in the documentation](https://rescript-lang.org/docs/gentype/latest/usage).|};
        ] );
      ( "genType.opaque",
        [
          {|The @genType decorator may be used to export ReScript values and types to JavaScript, and import JavaScript values and types into ReScript. It allows seamless integration of compiled ReScript modules in existing TypeScript, Flow, or plain JavaScript codebases, without loosing type information across different type systems.
    
[Read more and see examples in the documentation](https://rescript-lang.org/docs/gentype/latest/usage).|};
        ] );
      ( "get",
        [
          {|The `@get` decorator is used to bind to a property of an object.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#get-decorator).|};
        ] );
      ( "get_index",
        [
          {|The `@get_index` decorator is used to access a dynamic property on an object, or an index of an array.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#get-index-decorator).|};
        ] );
      ( "inline",
        [
          {|The `@inline` decorator tells the compiler to inline its value in every place the binding is being used, rather than use a variable.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#inline-decorator).|};
        ] );
      ( "int",
        [
          {|The `@int` decorator can be used with polymorphic variants and the @as decorator on externals to modify the compiled JavaScript to use integers for the values instead of strings.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#int-decorator).|};
        ] );
      ( "live",
        [
          {|The `@live` decorator is for reanalyze, a static analysis tool for ReScript that can do dead code analysis.

`@live` tells the dead code analysis that the value should be considered live, even though it might appear to be dead. This is typically used in case of FFI where there are indirect ways to access values. It can be added to everything that could otherwise be considered unused by the dead code analysis - values, functions, arguments, records, individual record fields, and so on.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#live-decorator).

Hint: Did you know you can run an interactive code analysis in your project by running the command `> ReScript: Start Code Analyzer`? Try it!|};
        ] );
      ( "meth",
        [
          {|The `@meth` decorator is used to call a function on a JavaScript object, and avoid issues with currying.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#meth-decorator).|};
        ] );
      ( "module",
        [
          {|The `@module` decorator is used to bind to a JavaScript module.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#module-decorator).|};
        ] );
      ( "new",
        [
          {|
The `@new` decorator is used whenever you need to bind to a JavaScript class constructor that requires the new keword for instantiation.|

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#new-decorator).|};
        ] );
      ( "obj",
        [
          {|The `@obj` decorator is used to create functions that return JavaScript objects with properties that match the function's parameter labels.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#obj-decorator).|};
        ] );
      ( "raises",
        [
          {|The `@raises` decorator is for reanalyze, a static analysis tool for ReScript that can perform exception analysis.

`@raises` acknowledges that a function can raise exceptions that are not caught, and suppresses
a warning in that case. Callers of the functions are then subjected to the same rule.
Example `@raises(Exn)` or `@raises([E1, E2, E3])` for multiple exceptions.
[Read more and see examples in the documentation](https://github.com/rescript-association/reanalyze/blob/master/EXCEPTION.md).
> Hint: Did you know you can run an interactive code analysis in your project by running the command `> ReScript: Start Code Analyzer`? Try it!|};
        ] );
      ( "react.component",
        [
          {|The `@react.component` decorator is used to annotate functions that are RescriptReact components.

You will need this decorator whenever you want to use a ReScript / React component in ReScript JSX expressions.

Note: The `@react.component` decorator requires the react-jsx config to be set in your `bsconfig.json` to enable the required React transformations.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#react-component-decorator).|};
        ] );
      ( "return",
        [
          {|The `@return` decorator is used to control how `null` and `undefined` values are converted to option types in ReScript.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#return-decorator).|};
        ] );
      ( "scope",
        [
          {|The `@scope` decorator is used with other decorators such as `@val` and `@module` to declare a parent scope for the binding.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#scope-decorator).|};
        ] );
      ( "send",
        [
          {|The `@send` decorator is used to bind to a method on an object or array.
      
[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#send-decorator).|};
        ] );
      ( "set",
        [
          {|The `@set` decorator is used to set a property of an object.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#set-decorator).|};
        ] );
      ( "set_index",
        [
          {|The `@set_index` decorator is used to set a dynamic property on an object, or an index of an array.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#set-index-decorator).|};
        ] );
      ( "string",
        [
          {|The `@string` decorator can be used with polymorphic variants and the `@as` decorator on externals to modify the string values used for the variants in the compiled JavaScript.
      
[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#string-decorator).|};
        ] );
      ( "this",
        [
          {|The `@this` decorator may be used to bind to an external callback function that require access to a this context.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#this-decorator).|};
        ] );
      ( "unboxed",
        [
          {|The `@unboxed` decorator provides a way to unwrap variant constructors that have a single argument, or record objects that have a single field.
      
[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#unboxed-decorator).|};
        ] );
      ( "uncurry",
        [
          {|The `@uncurry` decorator can be used to mark any callback argument within an external function as an uncurried function without the need for any explicit uncurried function syntax (`(.) => { ... }`).

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#uncurry-decorator).|};
        ] );
      ( "unwrap",
        [
          {|The `@unwrap` decorator may be used when binding to external functions that accept multiple types for an argument.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#unwrap-decorator).|};
        ] );
      ( "val",
        [
          {|The `@val` decorator allows you to bind to JavaScript values that are on the global scope.
      
[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#val-decorator).|};
        ] );
      ( "variadic",
        [
          {|The `@variadic` decorator is used to model JavaScript functions that take a variable number of arguments, where all arguments are of the same type.

[Read more and see examples in the documentation](https://rescript-lang.org/syntax-lookup#variadic-decorator).|};
        ] );
    ]
    |> List.filter (fun (decorator, _) -> Utils.startsWith decorator prefix)
    |> List.map (fun (decorator, doc) ->
           let parts = String.split_on_char '.' prefix in
           let len = String.length prefix in
           let dec2 =
             if List.length parts > 1 then
               String.sub decorator len (String.length decorator - len)
             else decorator
           in
           (dec2, doc))
    |> List.map mkDecorator
  | CnamedArg (cp, prefix, identsSeen) ->
    let labels =
      match
        cp
        |> getCompletionsForContextPath ~package ~opens ~rawOpens ~allFiles ~pos
             ~env ~exact:true ~scope
        |> completionsGetTypeEnv
      with
      | Some (typ, _env) ->
        if debug then
          Printf.printf "Found type for function %s\n"
            (typ |> Shared.typeToString);
        let rec getLabels ~env (t : Types.type_expr) =
          match t.desc with
          | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> getLabels ~env t1
          | Tarrow ((Labelled l | Optional l), tArg, tRet, _) ->
            (l, tArg) :: getLabels ~env tRet
          | Tarrow (Nolabel, _, tRet, _) -> getLabels ~env tRet
          | Tconstr (path, _, _) -> (
            match References.digConstructor ~env ~package path with
            | Some (env, {item = {decl = {type_manifest = Some t1}}}) ->
              getLabels ~env t1
            | _ -> [])
          | _ -> []
        in
        typ |> getLabels ~env
      | None -> []
    in
    let mkLabel (name, typ) =
      Completion.create ~name ~kind:(Label (typ |> Shared.typeToString)) ~env
    in
    labels
    |> List.filter (fun (name, _t) ->
           Utils.startsWith name prefix
           && (forHover || not (List.mem name identsSeen)))
    |> List.map mkLabel
  | CtypedContext
      {howToRetrieveSourceType; patternPath; meta = {prefix; alreadySeenIdents}}
    -> (
    let prefix =
      match prefix with
      | None -> ""
      | Some prefix -> prefix
    in
    let sourceType =
      howToRetrieveSourceType
      |> findSourceType ~package ~opens ~rawOpens ~allFiles ~env ~pos ~scope
    in
    let nestedContextPath =
      match patternPath with
      | None -> []
      | Some patternPath -> patternPath
    in
    match sourceType with
    | None -> []
    | Some typ -> (
      match typ |> findTypeInContext ~env ~nestedContextPath ~package with
      | None -> []
      | Some (CPolyVariant {constructors}) ->
        let constructorCompletions =
          constructors
          |> List.filter (fun constructor ->
                 Utils.startsWith constructor.name prefix)
          |> List.map (fun constructor ->
                 (* TODO: Can we leverage snippets here for automatically moving the cursor when there are multiple payloads?
                    Eg. Some($1) as completion item. *)
                 Completion.create
                   ~name:
                     ("#" ^ constructor.name
                     ^
                     if constructor.payload |> Option.is_some then "(_)" else ""
                     )
                   ~kind:(PolyvariantConstructor constructor) ~env)
        in
        let localCompletions =
          typ
          |> findLocalCompletionsForTypeExprWithOpens ~env ~prefix ~exact:false
               ~opens ~scope
        in
        constructorCompletions @ localCompletions
      | Some (CVariant {constructors}) ->
        if debug then Printf.printf "found variant: %s\n" prefix;
        let constructorCompletions =
          constructors
          |> List.filter (fun constructor ->
                 Utils.startsWith constructor.Constructor.cname.txt prefix
                 && not
                      (alreadySeenIdents
                      |> List.exists (fun alreadySeenConstructorName ->
                             alreadySeenConstructorName
                             = constructor.Constructor.cname.txt)))
          |> List.map (fun (constructor : Constructor.t) ->
                 (* TODO: Can we leverage snippets here for automatically moving the cursor when there are multiple payloads?
                    Eg. Some($1) as completion item. *)
                 Completion.create
                   ~name:
                     (constructor.cname.txt
                     ^ if constructor.args |> List.length > 0 then "(_)" else ""
                     )
                   ~kind:(Constructor (constructor, ""))
                   ~env)
        in
        let localCompletions =
          typ
          |> findLocalCompletionsForTypeExprWithOpens ~env ~prefix ~exact:false
               ~opens ~scope
        in
        constructorCompletions @ localCompletions
      | Some (CRecord {fields; decl; name}) ->
        fields
        |> List.filter (fun (field : field) ->
               not
                 (alreadySeenIdents
                 |> List.exists (fun fieldName -> fieldName = field.fname.txt)))
        |> Utils.filterMap (fun (field : field) ->
               if prefix = "" || checkName field.fname.txt ~prefix ~exact:false
               then
                 Some
                   (Completion.create ~name:field.fname.txt ~env
                      ~kind:
                        (Completion.Field
                           (field, decl |> Shared.declToString name.txt)))
               else None)))
