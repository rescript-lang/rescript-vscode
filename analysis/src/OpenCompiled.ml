let ( /+ ) = Filename.concat

let command ~path =
  let dir = Filename.dirname path in
  let projDir = FindFiles.findProjectRoot ~dir in
  match projDir with
  | None -> None
  | Some projDir -> (
    let bsconfig = projDir /+ "bsconfig.json" in
    match Files.readFile bsconfig with
    | None -> None
    | Some text -> (
      match Json.parse text with
      | None -> None
      | Some config -> (
        let pkgSpecs = config |> FindFiles.getPackageSpec in
        match pkgSpecs with
        | None -> None
        | Some {module_format; in_source; suffix} ->
          let compiledFolderName =
            match module_format with
            | "es6" -> "es6"
            | "es6-global" -> "es6_global"
            | _ -> "js"
          in
          let suffix_file =
            match suffix with
            | Some s -> s
            | None -> FindFiles.getSuffix config
          in
          let partialFileSplit = Files.split projDir path in
          let partialFilePath = List.nth partialFileSplit 0 in
          let pathFragment =
            if not in_source then "/lib" /+ compiledFolderName else ""
          in
          let compileFilePath =
            Filename.chop_extension partialFilePath ^ suffix_file
          in
          let path = projDir ^ pathFragment ^ compileFilePath in
          Some path)))