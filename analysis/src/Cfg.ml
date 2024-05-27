let debugFollowCtxPath = ref false

let isDocGenFromCompiler = ref false

let inIncrementalTypecheckingMode =
  ref
    (try
       match Sys.getenv "RESCRIPT_INCREMENTAL_TYPECHECKING" with
       | "true" -> true
       | _ -> false
     with _ -> false)
