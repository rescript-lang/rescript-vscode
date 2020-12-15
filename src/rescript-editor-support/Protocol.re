module J = JsonShort;

let pos = (~line, ~character) =>
  J.o([("line", J.i(line)), ("character", J.i(character))]);

open Infix;

let rgetPosition = pos => {
  open RResult.InfixResult;
  let%try line = RJson.get("line", pos) |?> RJson.number;
  let%try character = RJson.get("character", pos) |?> RJson.number;
  Ok((int_of_float(line), int_of_float(character)));
};

let rPositionParams = params => {
  open RResult.InfixResult;
  let%try uri =
    RJson.get("textDocument", params) |?> RJson.get("uri") |?> RJson.string;
  let%try pos = RJson.get("position", params) |?> rgetPosition;
  Ok((uri, pos));
};

let posOfLexing = ({Lexing.pos_lnum, pos_cnum, pos_bol}) =>
  J.o([
    ("line", J.i(pos_lnum - 1)),
    ("character", J.i(pos_cnum - pos_bol)),
  ]);

let contentKind = (useMarkdown, text) =>
  J.o([
    ("kind", J.s(useMarkdown ? "markdown" : "text")),
    ("value", J.s(text)),
  ]);

let rangeOfLoc = ({Location.loc_start, loc_end}) =>
  J.o([("start", posOfLexing(loc_start)), ("end", posOfLexing(loc_end))]);

let locationOfLoc =
    (~fname=?, {Location.loc_start: {Lexing.pos_fname}} as loc) =>
  J.o([
    ("range", rangeOfLoc(loc)),
    (
      "uri",
      J.s(
        switch (fname) {
        | Some(x) => x
        | None => Utils.toUri(pos_fname)
        },
      ),
    ),
  ]);

let rangeOfInts = (l0, c0, l1, c1) =>
  J.o([
    ("start", pos(~line=l0, ~character=c0)),
    ("end", pos(~line=l1, ~character=c1)),
  ]);

let locationContains = ({Location.loc_start, loc_end}, pos) =>
  Utils.tupleOfLexing(loc_start) <= pos
  && Utils.tupleOfLexing(loc_end) >= pos;

let symbolKind = (kind: SharedTypes.kinds) =>
  switch (kind) {
  | Module => 2
  | Enum => 10
  | Interface => 11
  | Function => 12
  | Variable => 13
  | Array => 18
  | Object => 19
  | Null => 21
  | EnumMember => 22
  | TypeParameter => 26
  };

/*
   returns true if a MarkupKind[] contains "markdown"
 */
let hasMarkdownCap = markupKind => {
  let%opt kinds = Json.array(markupKind) |?>> optMap(Json.string);
  Some(List.mem("markdown", kinds));
};
