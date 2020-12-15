let parseDependencyError = text => {
  let rx =
    Str.regexp(
      {|Error: The files \(.+\)\.cmi
       and \(.+\)\.cmi
       make inconsistent assumptions over interface \([A-Za-z_-]+\)|},
    );

  switch (Str.search_forward(rx, text, 0)) {
  | exception Not_found => None
  | _ =>
    let dep =
      Str.matched_group(1, text)
      |> Filename.basename
      |> String.capitalize_ascii;
    let base =
      Str.matched_group(2, text)
      |> Filename.basename
      |> String.capitalize_ascii;
    let interface = Str.matched_group(3, text);
    Some((dep, base, interface));
  };
};
