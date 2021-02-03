module Uri: {
  type t;
  let fromPath: string => t;
  let parse: string => option(t);
  let pathFromRoot: (~rootUri: t, t) => string;
  let toPath: t => string;
  let toString: t => string;
} = {
  type t = {
    path: string,
    uri: string,
  };

  let pathToUri = path =>
    if (Sys.os_type == "Unix") {
      "file://" ++ path;
    } else {
      "file://"
      ++ (
        Str.global_replace(Str.regexp_string("\\"), "/", path)
        |> Str.substitute_first(
             Str.regexp("^\\([A-Z]\\):"),
             text => {
               let name = Str.matched_group(1, text);
               "/" ++ String.lowercase_ascii(name) ++ "%3A";
             },
           )
      );
    };

  let fromPath = path => {path, uri: pathToUri(path)};

  let parseWindowsUri = withoutScheme => {
    withoutScheme
    |> Str.substitute_first(
         Str.regexp("^/\\([a-z]\\)%3A"),
         text => {
           let name = Str.matched_group(1, text);
           String.uppercase_ascii(name) ++ ":";
         },
       )
    /* OCaml doesn't want to do a find & replace where the replacement is a single backslash. So this works */
    |> Str.split(Str.regexp_string("/"))
    |> String.concat({|\|});
  };

  let parseUri = uri => {
    let withoutPct = Uri.pct_decode(uri);
    if (Utils.startsWith(withoutPct, "file://")) {
      let withoutScheme =
        Utils.sliceToEnd(withoutPct, String.length("file://"));

      if (Sys.os_type == "Unix") {
        Some(withoutScheme);
      } else {
        Some(parseWindowsUri(withoutScheme));
      };
    } else {
      None;
    };
  };

  let parse = uri =>
    switch (parseUri(uri)) {
    | Some(path) => Some({uri, path})
    | None => None
    };
  let toPath = ({path}) => path;
  let toString = ({uri}) => uri;

  let pathFromRoot = (~rootUri as {path: rootPath}, {path}) =>
    Utils.startsWith(path, rootPath)
      ? "<root>" ++ Utils.sliceToEnd(path, String.length(rootPath)) : path;
};

include Uri;
