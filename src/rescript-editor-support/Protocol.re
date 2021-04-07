module J = JsonShort;

let rgetPosition = pos => {
  open RResult.InfixResult;
  switch (RJson.get("line", pos) |?> RJson.number) {
  | Error(e) => Error(e)
  | Ok(line) =>
    switch (RJson.get("character", pos) |?> RJson.number) {
    | Error(e) => Error(e)
    | Ok(character) => Ok((int_of_float(line), int_of_float(character)))
    }
  }
};

let rPositionParams = params => {
  open RResult.InfixResult;
  switch (
    RJson.get("textDocument", params) |?> RJson.get("uri") |?> RJson.string
  ) {
  | Error(e) => Error(e)
  | Ok(uri) =>
    switch (Uri2.parse(uri) |> RResult.orError("Not a uri")) {
    | Error(e) => Error(e)
    | Ok(uri) =>
      switch (RJson.get("position", params) |?> rgetPosition) {
      | Error(e) => Error(e)
      | Ok(pos) => Ok((uri, pos))
      }
    }
  }
};

let posOfLexing = ({Lexing.pos_lnum, pos_cnum, pos_bol}) =>
  J.o([
    ("line", J.i(pos_lnum - 1)),
    ("character", J.i(pos_cnum - pos_bol)),
  ]);

let contentKind = text =>
  J.o([("kind", J.s("markdown")), ("value", J.s(text))]);

let rangeOfLoc = ({Location.loc_start, loc_end}) =>
  J.o([("start", posOfLexing(loc_start)), ("end", posOfLexing(loc_end))]);

let locationOfLoc =
    (~fname=?, {Location.loc_start: {Lexing.pos_fname}} as loc) => {
  let uri =
    switch (fname) {
    | Some(x) => x
    | None => Uri2.fromPath(pos_fname)
    };
  J.o([("range", rangeOfLoc(loc)), ("uri", J.s(Uri2.toString(uri)))]);
};

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
