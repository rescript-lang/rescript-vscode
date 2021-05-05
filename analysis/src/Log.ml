let spamError = ref false

let log msg = if !spamError then print_endline msg
