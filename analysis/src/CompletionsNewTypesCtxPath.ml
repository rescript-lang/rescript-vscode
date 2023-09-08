type completionCategory = Type | Value | Module | Field

type argumentLabel =
  | Unlabelled of {argumentPosition: int}
  | Labelled of string
  | Optional of string

type ctxPath =
  | CNone  (** Nothing. *)
  | CUnknown  (** Something that cannot be resolved right now *)
  | CId of string list * completionCategory
      (** A regular id of an expected category. `let fff = thisIsAnId<com>` and `let fff = SomePath.alsoAnId<com>` *)
  | CVariantPayload of {
      variantCtxPath: ctxPath;
      itemNum: int;
      constructorName: string;
    }
      (** A variant payload. `Some(<com>)` = itemNum 0, `Whatever(true, f<com>)` = itemNum 1 *)
  | CTupleItem of {tupleCtxPath: ctxPath; itemNum: int}
      (** A tuple item. `(true, false, <com>)` = itemNum 2 *)
  | CRecordField of {
      recordCtxPath: ctxPath;
      seenFields: string list;
      prefix: string;
    }
      (** A record field. `let f = {on: true, s<com>}` seenFields = [on], prefix = "s",*)
  (* TODO: Can we merge with CRecordFieldAccess?*)
  | CRecordFieldFollow of {recordCtxPath: ctxPath; fieldName: string}
      (** Follow this record field. {}*)
  | COption of ctxPath  (** An option with an inner type. *)
  | CArray of ctxPath option  (** An array with an inner type. *)
  | CTuple of ctxPath list  (** A tuple. *)
  | CBool
  | CString
  | CInt
  | CFloat
  | CRecordBody of {recordCtxPath: ctxPath; seenFields: string list}
  | CAwait of ctxPath  (** Awaiting a function call. *)
  | CFunction of {returnType: ctxPath}  (** A function *)
  | CRecordFieldAccess of {recordCtxPath: ctxPath; fieldName: string}
      (** Field access. `whateverVariable.fieldName`. The ctxPath points to the value of `whateverVariable`, 
          and the string is the name of the field we're accessing. *)
  | CObj of {objectCtxPath: ctxPath; propertyName: string}
      (** Object property access. `whateverVariable["fieldName"]`. The ctxPath points to the value of `whateverVariable`, 
          and the string is the name of the property we're accessing. *)
  | CApply of {functionCtxPath: ctxPath; args: Asttypes.arg_label list}
      (** Function application. `someFunction(someVar, ~otherLabel="hello")`. The ctxPath points to the function. *)
  | CFunctionArgument of {
      functionContextPath: ctxPath;
      argumentLabel: argumentLabel;
    }  (** A function argument, either labelled or unlabelled.*)
  | CPipe of {
      functionCtxPath: ctxPath;
          (** Context path to the function being called. *)
      id: string;
      lhsLoc: Location.t;  (** Location of the left hand side. *)
    }  (** Piped call. `foo->someFn`. *)
  | CJsxPropValue of {
      pathToComponent: string list;
          (** The path to the component this property is from. *)
      propName: string;  (** The prop name we're going through. *)
    }  (** A JSX property. *)
  | CTypeAtLoc of Location.t  (** A type at a location. *)
  | CFunctionReturnType of {functionCtxPath: ctxPath}
      (** An instruction to resolve the return type of the type at the 
      provided context path, if it's a function (it should always be, 
      but you know...) *)

let str s = if s = "" then "\"\"" else s
let list l = "[" ^ (l |> List.map str |> String.concat ", ") ^ "]"
let ident l = l |> List.map str |> String.concat "."

let rec ctxPathToString (ctxPath : ctxPath) =
  match ctxPath with
  | CUnknown -> "CUnknown"
  | CNone -> "CUnknown"
  | CBool -> "bool"
  | CFloat -> "float"
  | CInt -> "int"
  | CString -> "string"
  | CJsxPropValue {pathToComponent; propName} ->
    "<" ^ (pathToComponent |> list) ^ " =" ^ propName ^ "/>"
  | CAwait ctxPath -> Printf.sprintf "await (%s)" (ctxPathToString ctxPath)
  | CApply {functionCtxPath; args} ->
    Printf.sprintf "%s(%s)"
      (ctxPathToString functionCtxPath)
      (args
      |> List.map (function
           | Asttypes.Nolabel -> "Nolabel"
           | Labelled s -> "~" ^ s
           | Optional s -> "?" ^ s)
      |> String.concat ", ")
  | CRecordFieldAccess {recordCtxPath; fieldName} ->
    Printf.sprintf "(%s).%s" (ctxPathToString recordCtxPath) fieldName
  | CObj {objectCtxPath; propertyName} ->
    Printf.sprintf "(%s)[\"%s\"]" (ctxPathToString objectCtxPath) propertyName
  | CFunction {returnType} ->
    Printf.sprintf "() => %s" (ctxPathToString returnType)
  | CTuple ctxPaths ->
    Printf.sprintf "tuple(%s)"
      (ctxPaths |> List.map ctxPathToString |> String.concat ", ")
  | CId (prefix, typ) ->
    Printf.sprintf "%s(prefix=%s)"
      (match typ with
      | Value -> "Value"
      | Type -> "Type"
      | Module -> "Module"
      | Field -> "Field")
      (ident prefix)
  | CVariantPayload {variantCtxPath; itemNum; constructorName} ->
    Printf.sprintf "(%s)->variantPayload(%s<$%i>)"
      (ctxPathToString variantCtxPath)
      constructorName itemNum
  | CTupleItem {tupleCtxPath; itemNum} ->
    Printf.sprintf "%s->tupleItem($%i)" (ctxPathToString tupleCtxPath) itemNum
  | CRecordField {recordCtxPath; prefix; seenFields} ->
    Printf.sprintf "%s->recordField(\"%s\", %s)"
      (ctxPathToString recordCtxPath)
      prefix (seenFields |> list)
  | CRecordBody {recordCtxPath; seenFields} ->
    Printf.sprintf "%s->recordBody(%s)"
      (ctxPathToString recordCtxPath)
      (seenFields |> list)
  | COption ctxPath -> Printf.sprintf "option<%s>" (ctxPathToString ctxPath)
  | CArray ctxPath ->
    Printf.sprintf "array%s"
      (match ctxPath with
      | None -> ""
      | Some ctxPath -> "<" ^ ctxPathToString ctxPath ^ ">")
  | CFunctionArgument {functionContextPath; argumentLabel} ->
    "functionArgument("
    ^ (functionContextPath |> ctxPathToString)
    ^ ")("
    ^ (match argumentLabel with
      | Unlabelled {argumentPosition} -> "$" ^ string_of_int argumentPosition
      | Labelled name -> "~" ^ name
      | Optional name -> "~" ^ name ^ "=?")
    ^ ")"
  | CPipe {functionCtxPath; id} ->
    "pipe(" ^ ctxPathToString functionCtxPath ^ ")->" ^ id
  | CRecordFieldFollow {fieldName; recordCtxPath} ->
    Printf.sprintf "(%s)->followRecordField{%s}"
      (ctxPathToString recordCtxPath)
      fieldName
  | CTypeAtLoc loc -> Printf.sprintf "CTypeAtLoc: %s" (Loc.toString loc)
  | CFunctionReturnType {functionCtxPath} ->
    Printf.sprintf "returnTypeOf(%s)" (ctxPathToString functionCtxPath)
