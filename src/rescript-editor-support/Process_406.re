open SharedTypes;

let fileForCmt = (~moduleName, cmt, uri, processDoc) => {
  let%try infos = Shared.tryReadCmt(cmt);
  Ok(ProcessCmt.forCmt(~moduleName, uri, processDoc, infos));
};

let fullForCmt = (~moduleName, cmt, uri, processDoc) => {
  let%try infos = Shared.tryReadCmt(cmt);
  let file = ProcessCmt.forCmt(~moduleName, uri, processDoc, infos);
  let extra = ProcessExtra.forCmt(~file, infos);
  Ok({file, extra});
};

module PrintType = PrintType;
