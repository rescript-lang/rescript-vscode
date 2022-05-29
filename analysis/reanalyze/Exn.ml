type t = string

let compare = String.compare

let decodeError = "DecodeError"

let assertFailure = "Assert_failure"

let divisionByZero = "Division_by_zero"

let endOfFile = "End_of_file"

let exit = "exit"

let failure = "Failure"

let invalidArgument = "Invalid_argument"

let jsExnError = "Js.Exn.Error"

let matchFailure = "Match_failure"

let notFound = "Not_found"

let sysError = "Sys_error"

let fromLid lid = lid |> Longident.flatten |> String.concat "."

let fromString s = s

let toString s = s

let yojsonJsonError = "Yojson.Json_error"

let yojsonTypeError = "Yojson.Basic.Util.Type_error"