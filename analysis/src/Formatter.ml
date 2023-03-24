let format ~path =
  if Filename.check_suffix path ".res" then
    let {Res_driver.parsetree = structure; comments; diagnostics} =
      Res_driver.parsingEngine.parseImplementation ~forPrinter:true
        ~filename:path
    in
    if List.length diagnostics > 0 then None
    else
      Some
        (Res_printer.printImplementation ~width:!Res_cli.ResClflags.width
           ~comments structure)
  else if Filename.check_suffix path ".resi" then
    let {Res_driver.parsetree = signature; comments; diagnostics} =
      Res_driver.parsingEngine.parseInterface ~forPrinter:true ~filename:path
    in
    if List.length diagnostics > 0 then None
    else
      Some
        (Res_printer.printInterface ~width:!Res_cli.ResClflags.width ~comments
           signature)
  else None
