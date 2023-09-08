open SharedTypes
open CompletionsNewTypesCtxPath

module PositionContext = struct
  type t = {
    offset: int;  (** The offset *)
    cursor: Pos.t;  (** The actual position of the cursor *)
    beforeCursor: Pos.t;  (** The position just before the cursor *)
    noWhitespace: Pos.t;
        (** The position of the cursor, removing any whitespace _before_ it *)
    charBeforeNoWhitespace: char option;
        (** The first character before the cursor, excluding any whitespace *)
    charBeforeCursor: char option;
        (** The char before the cursor, not excluding whitespace *)
    whitespaceAfterCursor: char option;
        (** The type of whitespace after the cursor, if any *)
    locHasPos: Location.t -> bool;
        (** A helper for checking whether a loc has the cursor (beforeCursor). 
            This is the most natural position to check when figuring out if the user has the cursor in something. *)
  }

  let make ~offset ~posCursor text =
    let offsetNoWhite = Utils.skipWhite text (offset - 1) in
    let posNoWhite =
      let line, col = posCursor in
      (line, max 0 col - offset + offsetNoWhite)
    in
    let firstCharBeforeCursorNoWhite =
      if offsetNoWhite < String.length text && offsetNoWhite >= 0 then
        Some text.[offsetNoWhite]
      else None
    in
    let posBeforeCursor = Pos.posBeforeCursor posCursor in
    let charBeforeCursor, whitespaceAfterCursor =
      match Pos.positionToOffset text posCursor with
      | Some offset when offset > 0 -> (
        let charBeforeCursor = text.[offset - 1] in
        let charAtCursor =
          if offset < String.length text then text.[offset] else '\n'
        in
        match charAtCursor with
        | ' ' | '\t' | '\r' | '\n' ->
          (Some charBeforeCursor, Some charBeforeCursor)
        | _ -> (Some charBeforeCursor, None))
      | _ -> (None, None)
    in
    let locHasPos loc =
      loc |> CursorPosition.locHasCursor ~pos:posBeforeCursor
    in
    {
      offset;
      beforeCursor = posBeforeCursor;
      noWhitespace = posNoWhite;
      charBeforeNoWhitespace = firstCharBeforeCursorNoWhite;
      cursor = posCursor;
      charBeforeCursor;
      whitespaceAfterCursor;
      locHasPos;
    }
end

module CompletionContext = struct
  type t = {
    positionContext: PositionContext.t;
    scope: Scope.t;
    currentlyExpecting: ctxPath;
    ctxPath: ctxPath;
  }

  let make positionContext =
    {
      positionContext;
      scope = Scope.create ();
      currentlyExpecting = CNone;
      ctxPath = CNone;
    }

  let resetCtx completionContext =
    {completionContext with currentlyExpecting = CNone; ctxPath = CNone}

  let withScope scope completionContext = {completionContext with scope}

  let setCurrentlyExpecting currentlyExpecting completionContext =
    {completionContext with currentlyExpecting}

  let currentlyExpectingOrReset currentlyExpecting completionContext =
    match currentlyExpecting with
    | None -> {completionContext with currentlyExpecting = CNone}
    | Some currentlyExpecting -> {completionContext with currentlyExpecting}

  let currentlyExpectingOrTypeAtLoc ~loc currentlyExpecting completionContext =
    match currentlyExpecting with
    | None -> {completionContext with currentlyExpecting = CTypeAtLoc loc}
    | Some currentlyExpecting -> {completionContext with currentlyExpecting}

  let currentlyExpectingOrTypeAtLoc2 ~loc currentlyExpecting completionContext =
    let currentlyExpecting =
      match currentlyExpecting with
      | None -> CTypeAtLoc loc
      | Some currentlyExpecting -> currentlyExpecting
    in
    {completionContext with currentlyExpecting; ctxPath = currentlyExpecting}

  let withResetCurrentlyExpecting completionContext =
    {completionContext with currentlyExpecting = CNone}

  let addCtxPathItem ctxPath completionContext =
    {completionContext with ctxPath}
end

module CompletionInstruction = struct
  (** This is the completion instruction, that's responsible for resolving something at 
      context path X *)
  type t =
    | Cnone
    | CtxPath of ctxPath
    | Cpattern of {
        ctxPath: ctxPath;
            (** This is the context path inside of the pattern itself. 
              Used to walk up to the type we're looking to complete. *)
        rootType: ctxPath;
            (** This is the an instruction to find where completion starts 
              from. If we're completing inside of a record, it should resolve 
              to the record itself. *)
        prefix: string;
      }  (** Completing inside of a pattern. *)
    | Cexpression of {
        ctxPath: ctxPath;
            (** This is the context path inside of the expression itself. 
              Used to walk up to the type we're looking to complete. *)
        rootType: ctxPath;
            (** This is the an instruction to find where completion starts 
              from. If we're completing inside of a record, it should resolve 
              to the record itself. *)
        prefix: string;
      }  (** Completing inside of an expression. *)
    | CnamedArg of {
        ctxPath: ctxPath;
            (** Context path to the function with the argument. *)
        seenLabels: string list;
            (** All the already seen labels in the function call. *)
        prefix: string;  (** The text the user has written so far.*)
      }
    | Cjsx of {
        pathToComponent: string list;
            (** The path to the component: `["M", "Comp"]`. *)
        prefix: string;  (** What the user has already written. `"id"`. *)
        seenProps: string list;
            (** A list of all of the props that has already been entered.*)
      }
    | ChtmlElement of {prefix: string  (** What the user has written so far. *)}
        (** Completing for a regular HTML element. *)

  let ctxPath ctxPath = CtxPath ctxPath

  let pattern ~(completionContext : CompletionContext.t) ~prefix =
    Cpattern
      {
        prefix;
        rootType = completionContext.currentlyExpecting;
        ctxPath = completionContext.ctxPath;
      }

  let expression ~(completionContext : CompletionContext.t) ~prefix =
    Cexpression
      {
        prefix;
        rootType = completionContext.currentlyExpecting;
        ctxPath = completionContext.ctxPath;
      }

  let namedArg ~prefix ~functionContextPath ~seenLabels =
    CnamedArg {prefix; ctxPath = functionContextPath; seenLabels}

  let jsx ~prefix ~pathToComponent ~seenProps =
    Cjsx {prefix; pathToComponent; seenProps}

  let htmlElement ~prefix = ChtmlElement {prefix}

  let toString (c : t) =
    match c with
    | Cnone -> "Cnone"
    | CtxPath ctxPath -> Printf.sprintf "CtxPath: %s" (ctxPathToString ctxPath)
    | Cpattern {ctxPath; prefix} ->
      Printf.sprintf "Cpattern: ctxPath: %s, %s" (ctxPathToString ctxPath)
        (match prefix with
        | "" -> ""
        | prefix -> Printf.sprintf ", prefix: \"%s\"" prefix)
    | Cexpression {ctxPath; prefix} ->
      Printf.sprintf "Cexpression: ctxPath: %s %s" (ctxPathToString ctxPath)
        (match prefix with
        | "" -> ""
        | prefix -> Printf.sprintf ", prefix: \"%s\"" prefix)
    | CnamedArg {prefix; ctxPath; seenLabels} ->
      "CnamedArg("
      ^ (ctxPath |> ctxPathToString)
      ^ ", " ^ str prefix ^ ", " ^ (seenLabels |> list) ^ ")"
    | Cjsx {prefix; pathToComponent; seenProps} ->
      "Cjsx(" ^ (pathToComponent |> ident) ^ ", " ^ str prefix ^ ", "
      ^ (seenProps |> list) ^ ")"
    | ChtmlElement {prefix} -> "ChtmlElement <" ^ prefix ^ " />"
end

module CompletionResult = struct
  type t = (CompletionInstruction.t * CompletionContext.t) option

  let make (instruction : CompletionInstruction.t)
      (context : CompletionContext.t) =
    Some (instruction, context)

  let ctxPath (ctxPath : ctxPath) (completionContext : CompletionContext.t) =
    let completionContext =
      completionContext |> CompletionContext.addCtxPathItem ctxPath
    in
    make
      (CompletionInstruction.ctxPath completionContext.ctxPath)
      completionContext

  let pattern ~(completionContext : CompletionContext.t) ~prefix =
    make
      (CompletionInstruction.pattern ~completionContext ~prefix)
      completionContext

  let expression ~(completionContext : CompletionContext.t) ~prefix =
    make
      (CompletionInstruction.expression ~completionContext ~prefix)
      completionContext

  let namedArg ~(completionContext : CompletionContext.t) ~prefix ~seenLabels
      ~functionContextPath =
    make
      (CompletionInstruction.namedArg ~functionContextPath ~prefix ~seenLabels)
      completionContext

  let jsx ~(completionContext : CompletionContext.t) ~prefix ~pathToComponent
      ~seenProps =
    make
      (CompletionInstruction.jsx ~prefix ~pathToComponent ~seenProps)
      completionContext

  let htmlElement ~(completionContext : CompletionContext.t) ~prefix =
    make (CompletionInstruction.htmlElement ~prefix) completionContext
end
