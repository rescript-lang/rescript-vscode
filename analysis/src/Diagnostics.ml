let document_syntax ~path =
  let parse =
    Res_driver.parsingEngine.parseImplementation ~forPrinter:false
      ~filename:path
  in
  match parse.diagnostics with
  | [] -> []
  | diagnostics ->
    diagnostics
    |> List.rev_map (fun diagnostic ->
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
               severity = Error;
             })