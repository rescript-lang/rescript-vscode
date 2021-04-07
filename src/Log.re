let spamError = ref(false);

let log = msg =>
  if (spamError^) {
    output_string(stderr, msg ++ "\n");
    flush(stderr);
  };
