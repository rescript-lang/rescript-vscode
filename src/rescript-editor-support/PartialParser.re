let rec findBack = (text, char, i) =>
  if (i < 0) {
    0;
  } else if (text.[i] == char && (i == 0 || text.[i - 1] != '/')) {
    i - 1;
  } else {
    findBack(text, char, i - 1);
  };

let rec findOpenComment = (text, i) =>
  if (i < 1) {
    0;
  } else if (text.[i] == '*' && text.[i - 1] == '/') {
    i - 2;
  } else {
    findOpenComment(text, i - 1);
  };

let rec findBackSkippingCommentsAndStrings = (text, char, pair, i, level) => {
  let loop = findBackSkippingCommentsAndStrings(text, char, pair);
  if (i < 0) {
    0;
  } else if (text.[i] == char) {
    if (level == 0) {
      i - 1;
    } else {
      loop(i - 1, level - 1);
    };
  } else if (text.[i] == pair) {
    loop(i - 1, level + 1);
  } else {
    switch (text.[i]) {
    | '"' => loop(findBack(text, '"', i - 1), level)
    | '\'' => loop(findBack(text, '\'', i - 1), level)
    | '/' when i >= 1 && text.[i - 1] == '*' =>
      loop(findOpenComment(text, i - 2), level)
    | _ => loop(i - 1, level)
    };
  };
};

let rec skipWhite = (text, i) =>
  if (i < 0) {
    0;
  } else {
    switch (text.[i]) {
    | ' '
    | '\n'
    | '\t' => skipWhite(text, i - 1)
    | _ => i
    };
  };

let rec startOfLident = (text, i) =>
  if (i < 0) {
    0;
  } else {
    switch (text.[i]) {
    | 'a'..'z'
    | 'A'..'Z'
    | '.'
    | '_'
    | '0'..'9' => startOfLident(text, i - 1)
    | _ => i + 1
    };
  };

type completable =
  | Clabel(string)
  | Cpath(list(string));

let findCompletable = (text, offset) => {
  let mkPath = s => {
    let parts = Str.split(Str.regexp_string("."), s);
    let parts = s.[String.length(s) - 1] == '.' ? parts @ [""] : parts;
    Cpath(parts);
  };

  let rec loop = i => {
    i < 0
      ? Some(mkPath(String.sub(text, i + 1, offset - (i + 1))))
      : (
        switch (text.[i]) {
        | '~' => Some(Clabel(String.sub(text, i + 1, offset - (i + 1))))
        | 'a'..'z'
        | 'A'..'Z'
        | '0'..'9'
        | '.'
        | '_' => loop(i - 1)
        | _ =>
          i == offset - 1
            ? None : Some(mkPath(String.sub(text, i + 1, offset - (i + 1))))
        }
      );
  };
  if (offset > String.length(text) || offset == 0) {
    None;
  } else {
    loop(offset - 1);
  };
};

let findOpens = (text, offset) => {
  let opens = ref([]);
  let pathOfModuleOpen = o => {
    let rec loop = items =>
      switch (items) {
      | [] => SharedTypes.Tip("place holder")
      | [one, ...rest] => Nested(one, loop(rest))
      };
    loop(o |> Str.split(Str.regexp_string(".")));
  };
  let add = o => opens := [o |> pathOfModuleOpen, ...opens^];
  let maybeOpen = i0 => {
    let rec loop = i =>
      if (i < 4) {
        0;
      } else {
        switch (text.[i]) {
        | 'a'..'z'
        | 'A'..'Z'
        | '.'
        | '_'
        | '0'..'9' => loop(i - 1)
        | ' ' =>
          let at = skipWhite(text, i - 1);
          if (at >= 3
              && text.[at - 3] == 'o'
              && text.[at - 2] == 'p'
              && text.[at - 1] == 'e'
              && text.[at] == 'n') {
            add(String.sub(text, i + 1, i0 + 1 - (i + 1)));
            at - 4;
          } else {
            at;
          };
        | _ => i
        };
      };
    loop(i0 - 1);
  };

  let rec loop = i =>
    if (i > 1) {
      switch (text.[i]) {
      | '}' =>
        loop(findBackSkippingCommentsAndStrings(text, '{', '}', i - 1, 0))
      | ']' =>
        loop(findBackSkippingCommentsAndStrings(text, '[', ']', i - 1, 0))
      | ')' =>
        loop(findBackSkippingCommentsAndStrings(text, '(', ')', i - 1, 0))
      | '"' => loop(findBack(text, '"', i - 1))
      | 'a'..'z'
      | 'A'..'Z'
      | '_'
      | '0'..'9' => loop(maybeOpen(i))
      | '(' when text.[i - 1] == '.' =>
        switch (text.[i - 2]) {
        | 'a'..'z'
        | 'A'..'Z'
        | '_'
        | '0'..'9' =>
          let i0 = startOfLident(text, i - 3);
          add(String.sub(text, i0, i - i0 - 1));
        | _ => loop(i - 1)
        }
      | _ =>
        if (i > 1 && text.[i] == '/' && text.[i - 1] == '*') {
          loop(findOpenComment(text, i - 2));
        } else {
          loop(i - 1);
        }
      };
    };
  loop(offset - 1) |> ignore;
  opens^;
};

let offsetOfLine = (text, line) => {
  let ln = String.length(text);
  let rec loop = (i, lno) =>
    i >= ln
      ? None
      : (
        switch (text.[i]) {
        | '\n' => lno == line - 1 ? Some(i + 1) : loop(i + 1, lno + 1)
        | _ => loop(i + 1, lno)
        }
      );
  line == 0 ? Some(0) : loop(0, 0);
};

let positionToOffset = (text, (line, character)) => {
  Infix.(offsetOfLine(text, line) |?>> (bol => bol + character));
};
