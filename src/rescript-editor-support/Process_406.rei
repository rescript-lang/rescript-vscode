let fileForCmt:
  (~moduleName: string, ~uri: Uri2.t, string, string => string) =>
  result(SharedTypes.file, string);

let fullForCmt:
  (~moduleName: string, ~uri: Uri2.t, string, string => string) =>
  result(SharedTypes.full, string);
