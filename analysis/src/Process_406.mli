val fileForCmt :
  moduleName:string ->
  uri:Uri2.t ->
  string ->
  (string -> string list) ->
  (SharedTypes.file, string) result

val fullForCmt :
  moduleName:string ->
  uri:Uri2.t ->
  string ->
  (string -> string list) ->
  (SharedTypes.full, string) result
