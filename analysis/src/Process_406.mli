val fileForCmt :
  moduleName:string -> uri:Uri2.t -> string -> (SharedTypes.file, string) result

val fullForCmt :
  moduleName:string -> uri:Uri2.t -> string -> (SharedTypes.full, string) result
