let split = (str, string) => Str.split(Str.regexp_string(str), string);

let removeExtraDots = path =>
  Str.global_replace(Str.regexp_string("/./"), "/", path)
  |> Str.global_replace(Str.regexp({|^\./\.\./|}), "../");

// Win32 & MacOS are case-insensitive
let pathEq =
  Sys.os_type == "Linux"
    ? (a, b) => a == b
    : ((a, b) => String.lowercase_ascii(a) == String.lowercase_ascii(b));

let pathStartsWith = (text, prefix) =>
  String.length(prefix) <= String.length(text)
  && pathEq(String.sub(text, 0, String.length(prefix)), prefix);
let sliceToEnd = (str, pos) =>
  String.sub(str, pos, String.length(str) - pos);

let relpath = (base, path) =>
  if (pathStartsWith(path, base)) {
    let baselen = String.length(base);
    let rest = String.sub(path, baselen, String.length(path) - baselen);
    if (rest == "") {
      "." ++ Filename.dir_sep;
    } else if (rest.[0] == Filename.dir_sep.[0]) {
      if (String.length(rest) > 1 && rest.[1] == '.') {
        sliceToEnd(rest, 1);
      } else {
        "." ++ rest;
      };
    } else if (rest.[0] == '.') {
      rest;
    } else {
      "." ++ Filename.dir_sep ++ rest;
    };
  } else {
    let rec loop = (bp, pp) => {
      switch (bp, pp) {
      | ([".", ...ra], _) => loop(ra, pp)
      | (_, [".", ...rb]) => loop(bp, rb)
      | ([a, ...ra], [b, ...rb]) when pathEq(a, b) => loop(ra, rb)
      | _ => (bp, pp)
      };
    };
    let (base, path) =
      loop(split(Filename.dir_sep, base), split(Filename.dir_sep, path));
    String.concat(
      Filename.dir_sep,
      (base == [] ? ["."] : List.map(_ => "..", base)) @ path,
    )
    |> removeExtraDots;
  };

let maybeStat = path =>
  try(Some(Unix.stat(path))) {
  | Unix.Unix_error(Unix.ENOENT, _, _) => None
  };

let getMtime = path =>
  switch (maybeStat(path)) {
  | Some({Unix.st_mtime}) => Some(st_mtime)
  | _ => None
  };

let readFile = path => {
  switch (maybeStat(path)) {
  | Some({Unix.st_kind: Unix.S_REG}) =>
    let ic = open_in(path);
    let try_read = () =>
      switch (input_line(ic)) {
      | exception End_of_file => None
      | x => Some(x)
      };
    let rec loop = acc =>
      switch (try_read()) {
      | Some(s) => loop([s, ...acc])
      | None =>
        close_in(ic);
        List.rev(acc);
      };
    let text = loop([]) |> String.concat(String.make(1, '\n'));
    Some(text);
  | _ => None
  };
};

let readFileResult = path =>
  switch (readFile(path)) {
  | None => Error("Unable to read " ++ path)
  | Some(text) => Ok(text)
  };

let exists = path =>
  switch (maybeStat(path)) {
  | None => false
  | Some(_) => true
  };

let ifExists = path => exists(path) ? Some(path) : None;

let readDirectory = dir => {
  let maybeGet = handle =>
    try(Some(Unix.readdir(handle))) {
    | End_of_file => None
    };
  let rec loop = handle =>
    switch (maybeGet(handle)) {
    | None =>
      Unix.closedir(handle);
      [];
    | Some(name)
        when
          name == Filename.current_dir_name || name == Filename.parent_dir_name =>
      loop(handle)
    | Some(name) => [name, ...loop(handle)]
    };
  switch (Unix.opendir(dir)) {
  | exception (Unix.Unix_error(Unix.ENOENT, "opendir", _dir)) => []
  | handle => loop(handle)
  };
};

let rec mkdirp = dest =>
  if (!exists(dest)) {
    let parent = Filename.dirname(dest);
    mkdirp(parent);
    Unix.mkdir(dest, 0o740);
    if (!exists(dest)) {
      failwith("Unable to create " ++ dest);
    };
  };

let rec collectDirs = path => {
  switch (maybeStat(path)) {
  | None => []
  | Some({Unix.st_kind: Unix.S_DIR}) => [
      path,
      ...readDirectory(path)
         |> List.map(name => collectDirs(Filename.concat(path, name)))
         |> List.concat,
    ]
  | _ => []
  };
};

let rec collect = (~checkDir=_ => true, path, test) =>
  switch (maybeStat(path)) {
  | None => []
  | Some({Unix.st_kind: Unix.S_DIR}) =>
    if (checkDir(path)) {
      readDirectory(path)
      |> List.map(name =>
           collect(~checkDir, Filename.concat(path, name), test)
         )
      |> List.concat;
    } else {
      [];
    }
  | _ => test(path) ? [path] : []
  };

let fileConcat = (a, b) =>
  if (b != ""
      && b.[0] == '.'
      && String.length(b) >= 2
      && b.[1] == Filename.dir_sep.[0]) {
    Filename.concat(a, String.sub(b, 2, String.length(b) - 2));
  } else {
    Filename.concat(a, b);
  };

let isFullPath = b =>
  b.[0] == '/' || Sys.win32 && String.length(b) > 1 && b.[1] == ':';

let maybeConcat = (a, b) =>
  if (b != "" && isFullPath(b)) {
    b;
  } else {
    fileConcat(a, b);
  };
