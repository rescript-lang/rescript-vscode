let fileForCmt:
  (~moduleName: string, ~uri: Uri2.t, string, string => list(string)) =>
  result(SharedTypes.file, string);

let fullForCmt:
  (~moduleName: string, ~uri: Uri2.t, string, string => list(string)) =>
  result(SharedTypes.full, string);
