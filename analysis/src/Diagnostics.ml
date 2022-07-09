let document_syntax ~path =
  let get_diagnostics diagnostics =
    diagnostics
    |> List.map (fun diagnostic ->
           let _, startline, startcol =
             Location.get_pos_info (Res_diagnostics.getStartPos diagnostic)
           in
           let _, endline, endcol =
             Location.get_pos_info (Res_diagnostics.getEndPos diagnostic)
           in
           Protocol.stringifyDiagnostic
             {
               range =
                 {
                   start = {line = startline - 1; character = startcol};
                   end_ = {line = endline - 1; character = endcol};
                 };
               message = Res_diagnostics.explain diagnostic;
               severity = 1;
             })
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
