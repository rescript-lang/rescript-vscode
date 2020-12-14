let out = ref(None);

let initial_dest = Filename.concat(Filename.get_temp_dir_name(), "lsp.log");

let setLocation = location => {
  switch (out^) {
  | None => ()
  | Some(out) => close_out(out)
  };
  output_string(stderr, "Setting log location: " ++ location ++ "\n");
  flush(stderr);
  out := Some(open_out(location));
};

let spamError = ref(false);

let log = msg => {
  if (spamError^) {
    output_string(stderr, msg ++ "\n");
    flush(stderr);
  };
  switch (out^) {
  | None => ()
  | Some(out) =>
    output_string(out, msg ++ "\n");
    flush(out);
  };
};
