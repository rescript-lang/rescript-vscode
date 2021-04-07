module Uri: {
  type t;
  let fromPath: string => t;
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
             Str.regexp("^\\([a-zA-Z]\\):"),
             text => {
               let name = Str.matched_group(1, text);
               "/" ++ String.lowercase_ascii(name) ++ "%3A";
             },
           )
      );
    };

  let fromPath = path => {path, uri: pathToUri(path)};

  let toPath = ({path}) => path;
  let toString = ({uri}) => uri;
};

include Uri;
