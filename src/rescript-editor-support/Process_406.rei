let fileForCmt:
  (~moduleName: string, string, string, string => string) =>
  result(SharedTypes.file, string);

let fullForCmt:
  (~moduleName: string, string, string, string => string) =>
  result(SharedTypes.full, string);
