let spamError = ref false

let log msg =
  if !spamError then (
    output_string stderr (msg ^ "\n");
    flush stderr )
