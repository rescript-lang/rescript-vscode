let document_syntax ~path =
  let get_diagnostics diagnostics =
    diagnostics
    |> List.map (fun diagnostic ->
           let start =
             Utils.cmtPosToPosition (Res_diagnostics.getStartPos diagnostic)
           in
           let end_ =
             Utils.cmtPosToPosition (Res_diagnostics.getEndPos diagnostic)
           in
           Protocol.{range = {start; end_}; message = diagnostic; severity = 1})
  in
  if FindFiles.isImplementation path then
    let parseImplementation =
      Res_driver.parsingEngine.parseImplementation ~forPrinter:false
        ~filename:path
    in
    get_diagnostics parseImplementation.diagnostics
  else if FindFiles.isInterface path then
    let parseInterface =
      Res_driver.parsingEngine.parseInterface ~forPrinter:false ~filename:path
    in
    get_diagnostics parseInterface.diagnostics
  else []
