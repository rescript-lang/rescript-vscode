type debugLevel = Off | Regular | Verbose

let debugLevel = ref Off

let log s =
  match !debugLevel with
  | Regular | Verbose -> print_endline s
  | Off -> ()

let logVerbose s = if !debugLevel = Verbose then print_endline s
