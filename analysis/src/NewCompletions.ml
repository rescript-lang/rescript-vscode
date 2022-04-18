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
          | [] | [_] -> previous
          | name :: path -> (
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

let checkName name ~prefix ~exact =
  if exact then name = prefix else Utils.startsWith name prefix

let completionForDeclareds ~pos ~iter ~stamps ~prefix ~exact ~env
    transformContents =
  (* Log.log("completion for declares " ++ prefix); *)
  let res = ref [] in
  iter
    (fun _stamp (declared : _ Declared.t) ->
      if
        checkName declared.name.txt ~prefix ~exact
        && Utils.locationContainsFuzzy declared.scopeLoc pos
      then
        res :=
          {
            (Completion.create ~name:declared.name.txt ~env
               ~kind:(transformContents declared.item))
            with
            extentLoc = declared.extentLoc;
            deprecated = declared.deprecated;
            docstring = declared.docstring;
          }
          :: !res)
    stamps;
  !res

let completionForDeclaredModules ~pos ~env ~prefix ~exact =
  completionForDeclareds ~env ~pos ~iter:Stamps.iterModules
    ~stamps:env.QueryEnv.file.stamps ~prefix ~exact (fun m ->
      Completion.Module m)

let completionForDeclaredValues ~pos ~env ~prefix ~exact =
  completionForDeclareds ~env ~pos ~iter:Stamps.iterValues
    ~stamps:env.QueryEnv.file.stamps ~prefix ~exact (fun m ->
      Completion.Value m)

let completionForDeclaredTypes ~pos ~env ~prefix ~exact =
  completionForDeclareds ~env ~pos ~iter:Stamps.iterTypes
    ~stamps:env.QueryEnv.file.stamps ~prefix ~exact (fun m -> Completion.Type m)

let completionForExporteds iterExported getDeclared ~prefix ~exact ~env
    transformContents =
  let res = ref [] in
  iterExported (fun name stamp ->
      (* Log.log("checking exported: " ++ name); *)
      if checkName name ~prefix ~exact then
        match getDeclared stamp with
        | Some (declared : _ Declared.t) ->
          res :=
            {
              (Completion.create ~name:declared.name.txt ~env
                 ~kind:(transformContents declared.item))
              with
              extentLoc = declared.extentLoc;
              deprecated = declared.deprecated;
              docstring = declared.docstring;
            }
            :: !res
        | None -> ());
  !res

let completionForExportedModules ~env ~prefix ~exact =
  completionForExporteds (Exported.iter env.QueryEnv.exported Exported.Module)
    (Stamps.findModule env.file.stamps) ~prefix ~exact ~env (fun m ->
      Completion.Module m)

let completionForExportedValues ~env ~prefix ~exact =
  completionForExporteds (Exported.iter env.QueryEnv.exported Exported.Value)
    (Stamps.findValue env.file.stamps) ~prefix ~exact ~env (fun v ->
      Completion.Value v)

let completionForExportedTypes ~env ~prefix ~exact =
  completionForExporteds (Exported.iter env.QueryEnv.exported Exported.Type)
    (Stamps.findType env.file.stamps) ~prefix ~exact ~env (fun t ->
      Completion.Type t)

let completionForConstructors ~(env : QueryEnv.t) ~prefix ~exact =
  let res = ref [] in
  Exported.iter env.exported Exported.Type (fun _name stamp ->
      match Stamps.findType env.file.stamps stamp with
      | Some ({item = {kind = Type.Variant constructors}} as t) ->
        res :=
          (constructors
          |> List.filter (fun c ->
                 checkName c.Constructor.cname.txt ~prefix ~exact)
          |> List.map (fun c ->
                 Completion.create ~name:c.Constructor.cname.txt ~env
                   ~kind:
                     (Completion.Constructor
                        (c, t.item.decl |> Shared.declToString t.name.txt))))
          @ !res
      | _ -> ());
  !res

let completionForFields ~(env : QueryEnv.t) ~prefix ~exact =
  let res = ref [] in
  Exported.iter env.exported Exported.Type (fun _name stamp ->
      match Stamps.findType env.file.stamps stamp with
      | Some ({item = {kind = Record fields}} as t) ->
        res :=
          (fields
          |> List.filter (fun f -> checkName f.fname.txt ~prefix ~exact)
          |> List.map (fun f ->
                 Completion.create ~name:f.fname.txt ~env
                   ~kind:
                     (Completion.Field
                        (f, t.item.decl |> Shared.declToString t.name.txt))))
          @ !res
      | _ -> ());
  !res

(* Note: This is a hack. It will be wrong some times if you have a local thing
   that overrides an open.

   Maybe the way to fix it is to make note of what things in an open override
   locally defined things...
*)
let getEnvWithOpens ~pos ~(env : QueryEnv.t) ~package ~(opens : QueryEnv.t list)
    (path : string list) =
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
        | [] | [_] -> None
        | top :: path -> (
          Log.log ("Getting module " ^ top);
          match ProcessCmt.fileForModule ~package top with
          | None -> None
          | Some file ->
            Log.log "got it";
            let env = QueryEnv.fromFile file in
            ProcessCmt.resolvePath ~env ~package ~path))
    in
    loop opens

let detail name (kind : Completion.kind) =
  match kind with
  | Type {decl} -> decl |> Shared.declToString name
  | Value typ -> typ |> Shared.typeToString
  | Module _ -> "module"
  | FileModule _ -> "file module"
  | Field ({typ}, s) -> name ^ ": " ^ (typ |> Shared.typeToString) ^ "\n\n" ^ s
  | Constructor (c, s) -> showConstructor c ^ "\n\n" ^ s

let localCompletions ~pos ~(env : QueryEnv.t) ~prefix ~exact =
  Log.log "---------------- LOCAL VAL";
  completionForDeclaredModules ~pos ~env ~prefix ~exact
  @ completionForConstructors ~env ~prefix ~exact
  @ completionForDeclaredValues ~pos ~env ~prefix ~exact
  @ completionForDeclaredTypes ~pos ~env ~prefix ~exact
  @ completionForFields ~env ~prefix ~exact

let allCompletions ~(env : QueryEnv.t) ~prefix ~exact =
  Log.log (" - Completing in " ^ Uri2.toString env.file.uri);
  completionForExportedModules ~env ~prefix ~exact
  @ completionForConstructors ~env ~prefix ~exact
  @ completionForExportedValues ~env ~prefix ~exact
  @ completionForExportedTypes ~env ~prefix ~exact
  @ completionForFields ~env ~prefix ~exact

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

let filterCompletionKind ~(completionContext : PartialParser.completionContext)
    {Completion.kind} =
  match (completionContext, kind) with
  | Module, (Module _ | FileModule _) -> true
  | Module, _ -> false
  | (Field | Type | Value), (Module _ | FileModule _) ->
    (* M.field M.type M.value *)
    true
  | Value, (Value _ | Constructor _) ->
    (* x Red *)
    true
  | Value, _ -> false
  | Field, Field _ -> true
  | Field, _ -> false
  | Type, Type _ -> true
  | Type, _ -> false

let prioritize ~exact ~pos completions =
  if exact then
    (* Heuristic to approximate scope when an exact name is required and there could
        be more than one instance of that name.
       Take the last position before pos if any, or just return the first element. *)
    let rec loop decls =
      match decls with
      | d1 :: d2 :: rest ->
        let pos2 = d2.Completion.extentLoc.loc_start |> Pos.ofLexing in
        if pos2 >= pos then loop (d1 :: rest)
        else
          let pos1 = d1.extentLoc.loc_start |> Pos.ofLexing in
          if pos1 <= pos2 then loop (d2 :: rest) else loop (d1 :: rest)
      | [] | [_] -> decls
    in
    loop completions
  else completions

let postProcess ~pos ~exact ~completionContext completions =
  completions
  |> List.filter (filterCompletionKind ~completionContext)
  |> prioritize ~exact ~pos

let getCompletionsForPath ~package ~opens ~allFiles ~pos ~exact
    ~completionContext ~env path =
  match path with
  | [] -> []
  | [prefix] ->
    let locallyDefinedValues = localCompletions ~pos ~env ~prefix ~exact in
    let alreadyUsedIdentifiers = Hashtbl.create 10 in
    let valuesFromOpens =
      opens
      |> List.fold_left
           (fun results env ->
             let completionsFromThisOpen = allCompletions ~env ~prefix ~exact in
             List.filter
               (fun (declared : Completion.t) ->
                 if Hashtbl.mem alreadyUsedIdentifiers declared.name then
                   (* shadowing from opens *)
                   false
                 else (
                   Hashtbl.add alreadyUsedIdentifiers declared.name true;
                   true))
               completionsFromThisOpen
             @ results)
           []
    in
    (* TODO complete the namespaced name too *)
    let localModuleNames =
      allFiles |> FileSet.elements
      |> Utils.filterMap (fun name ->
             if checkName name ~prefix ~exact && not (String.contains name '-')
             then
               Some
                 (Completion.create ~name ~env
                    ~kind:(Completion.FileModule name))
             else None)
    in
    locallyDefinedValues @ valuesFromOpens @ localModuleNames
    |> postProcess ~pos ~exact ~completionContext
  | _ -> (
    Log.log ("Path " ^ pathToString path);
    match getEnvWithOpens ~pos ~env ~package ~opens path with
    | Some (env, prefix) ->
      Log.log "Got the env";
      allCompletions ~env ~prefix ~exact
      |> postProcess ~pos ~exact ~completionContext
    | None -> [])

let getCompletionsForRecordAccess ~package ~opens ~pos ~exact ~env
    (valuePath, middleFields, lastField) =
  Log.log ("lastField :" ^ lastField);
  Log.log ("-------------- Looking for " ^ (valuePath |> pathToString));
  match getEnvWithOpens ~pos ~env ~package ~opens valuePath with
  | Some (env, name) -> (
    match ProcessCmt.findInScope pos name Stamps.iterValues env.file.stamps with
    | None -> []
    | Some declared -> (
      Log.log ("Found it! " ^ declared.name.txt);
      match declared.item |> extractRecordType ~env ~package with
      | None -> []
      | Some (env, fields, typDecl) -> (
        match
          middleFields
          |> List.fold_left
               (fun current name ->
                 match current with
                 | None -> None
                 | Some (env, fields, _) -> (
                   match
                     fields |> List.find_opt (fun f -> f.fname.txt = name)
                   with
                   | None -> None
                   | Some field ->
                     Log.log ("Found field " ^ name);
                     field.typ |> extractRecordType ~env ~package))
               (Some (env, fields, typDecl))
        with
        | None -> []
        | Some (env, fields, typDecl) ->
          fields
          |> Utils.filterMap (fun field ->
                 if checkName field.fname.txt ~prefix:lastField ~exact then
                   Some
                     (Completion.create ~name:field.fname.txt ~env
                        ~kind:
                          (Completion.Field
                             ( field,
                               typDecl.item.decl
                               |> Shared.declToString typDecl.name.txt )))
                 else None))))
  | None -> []

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

let completionToItem {Completion.name; deprecated; docstring; kind} =
  mkItem ~name
    ~kind:(Completion.kindToInt kind)
    ~deprecated ~detail:(detail name kind) ~docstring

let processDotPath ~package ~opens ~allFiles ~pos ~env
    (dotpath, completionContext) =
  let completion = dotpath |> PartialParser.determineCompletion in
  match completion with
  | Path path ->
    path
    |> getCompletionsForPath ~package ~opens ~allFiles ~pos ~exact:false
         ~completionContext ~env
  | RecordAccess (valuePath, middleFields, lastField) ->
    (valuePath, middleFields, lastField)
    |> getCompletionsForRecordAccess ~package ~opens ~pos ~exact:false ~env

let completionsGetTypeEnv = function
  | {Completion.kind = Value typ; env} :: _ -> Some (typ, env)
  | {Completion.kind = Field ({typ}, _); env} :: _ -> Some (typ, env)
  | _ -> None

let processCompletable ~full ~package ~rawOpens ~opens ~env ~pos
    (completable : PartialParser.completable) =
  let allFiles = FileSet.union package.projectFiles package.dependenciesFiles in
  let findTypeOfValue path =
    path
    |> getCompletionsForPath ~completionContext:PartialParser.Value ~exact:true
         ~package ~opens ~allFiles ~pos ~env
    |> completionsGetTypeEnv
  in
  match completable with
  | Cpath (CPId (path, completionContext)) ->
    path
    |> getCompletionsForPath ~package ~opens ~allFiles ~pos ~exact:false
         ~completionContext ~env
    |> List.map completionToItem
  | Cpath _ -> assert false
  | Cdotpath (dotpath, completionContext) ->
    processDotPath ~package ~opens ~allFiles ~pos ~env
      (dotpath, completionContext)
    |> List.map completionToItem
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
    let getObjectFields ~env (t : Types.type_expr) =
      match t |> extractObjectType ~env ~package with
      | Some (env, tObj) -> (env, getFields tObj)
      | None -> (env, [])
    in
    let rec resolvePath ~env fields path =
      match path with
      | name :: restPath -> (
        match fields |> List.find_opt (fun (n, _) -> n = name) with
        | Some (_, fieldType) ->
          let env, innerFields = getObjectFields ~env fieldType in
          resolvePath ~env innerFields restPath
        | None -> [])
      | [] -> fields
    in
    let env0 = QueryEnv.fromFile full.file in
    let env, fields =
      match lhs |> findTypeOfValue with
      | Some (typ, env) -> getObjectFields ~env typ
      | None -> (env0, [])
    in
    let labels = resolvePath ~env fields path in
    let mkLabel_ name typString =
      mkItem ~name ~kind:4 ~deprecated:None ~detail:typString ~docstring:[]
    in
    let mkLabel (name, typ) = mkLabel_ name (typ |> Shared.typeToString) in
    if labels = [] then []
    else
      labels
      |> List.filter (fun (name, _t) -> Utils.startsWith name prefix)
      |> List.map mkLabel
  | Cjsx ([id], prefix, identsSeen) when String.uncapitalize_ascii id = id ->
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
    let labels =
      match componentPath @ ["make"] |> findTypeOfValue with
      | Some (typ, _env) ->
        let rec getFields (texp : Types.type_expr) =
          match texp.desc with
          | Tfield (name, _, t1, t2) ->
            let fields = t2 |> getFields in
            if name = "children" then fields else (name, t1) :: fields
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
      | None -> []
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
    let getLhsPath ~pipeIdPath ~partialName =
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
        match extractRecordType typ ~env ~package with
        | Some (env1, fields, _) -> (
          match
            fields |> List.find_opt (fun field -> field.fname.txt = fieldName)
          with
          | None -> None
          | Some field -> Some (field.typ, env1))
        | None -> None
      in
      let rec getFields ~env ~typ = function
        | [] -> Some (typ, env)
        | fieldName :: rest -> (
          match getField ~env ~typ fieldName with
          | None -> None
          | Some (typ1, env1) -> getFields ~env:env1 ~typ:typ1 rest)
      in
      match pipeIdPath with
      | x :: fieldNames -> (
        match [x] |> findTypeOfValue with
        | Some (typ, env) -> (
          match getFields ~env ~typ fieldNames with
          | None -> None
          | Some (typ1, _env1) -> fromType typ1)
        | None -> None)
      | [] -> None
    in
    let lhsPath =
      match pipe with
      | PipeId pipeIdPath -> getLhsPath ~pipeIdPath ~partialName
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
      | [_], _ -> Some modulePath
      | s :: inner, first :: restPath when s = first ->
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
        let declareds =
          modulePath @ [partialName]
          |> getCompletionsForPath ~completionContext:PartialParser.Value
               ~exact:false ~package ~opens ~allFiles ~pos ~env
        in
        declareds
        |> List.map (fun {Completion.name; deprecated; docstring; kind} ->
               mkItem ~name:(completionName name)
                 ~kind:(Completion.kindToInt kind)
                 ~detail:(detail name kind) ~deprecated ~docstring)
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
      match funPath |> findTypeOfValue with
      | Some (typ, _env) ->
        let rec getLabels (t : Types.type_expr) =
          match t.desc with
          | Tlink t1 | Tsubst t1 -> getLabels t1
          | Tarrow ((Labelled l | Optional l), tArg, tRet, _) ->
            (l, tArg) :: getLabels tRet
          | Tarrow (Nolabel, _, tRet, _) -> getLabels tRet
          | _ -> []
        in
        typ |> getLabels
      | None -> []
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

let computeCompletions ~(completable : PartialParser.completable) ~full ~pos
    ~rawOpens ~opens ~env =
  let package = full.package in

  let rec processContextPath (cp : PartialParser.contextPath) :
      PartialParser.completable =
    match cp with
    | CPId (path, k) -> PartialParser.Cdotpath (path, k)
    | CPField (cp, name) -> (
      match processContextPath cp with
      | Cdotpath (path, _) -> Cdotpath (path @ [name], Field)
      | _ -> assert false)
    | CPObj (cp, objLabel) -> (
      match processContextPath cp with
      | Cdotpath (path, _k) -> Cobj (path, [], objLabel)
      | Cobj (path, objPath, label) -> Cobj (path, objPath @ [label], objLabel)
      | _ -> assert false)
  in

  let completable =
    match completable with
    | Cobj _ -> assert false
    | Cdotpath _ -> assert false
    | Cpath contextPath -> processContextPath contextPath
    | _ -> completable
  in

  completable |> processCompletable ~full ~package ~rawOpens ~opens ~env ~pos
