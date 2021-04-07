open SharedTypes;

let fileForCmt = (~moduleName, ~uri, cmt, processDoc) =>
  switch (Shared.tryReadCmt(cmt)) {
  | Error(e) => Error(e)
  | Ok(infos) => Ok(ProcessCmt.forCmt(~moduleName, ~uri, processDoc, infos))
  };

let fullForCmt = (~moduleName, ~uri, cmt, processDoc) =>
  switch (Shared.tryReadCmt(cmt)) {
  | Error(e) => Error(e)
  | Ok(infos) =>
    let file = ProcessCmt.forCmt(~moduleName, ~uri, processDoc, infos);
    let extra = ProcessExtra.forCmt(~file, infos);
    Ok({file, extra});
  };

module PrintType = PrintType;
