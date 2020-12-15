type result =
  /* | ParseError(string) */
  | SyntaxError(string, string, SharedTypes.full)
  | TypeError(string, SharedTypes.full)
  | Success(string, SharedTypes.full);

open Infix;

let getResult = result =>
  switch (result) {
  | SyntaxError(_, _, data) => data
  | TypeError(_, data) => data
  | Success(_, data) => data
  };

let runRefmt = (~interface, ~moduleName, ~cacheLocation, text, refmt) => {
  let target = cacheLocation /+ moduleName ++ ".ast" ++ (interface ? "i" : "");
  let cmd =
    Printf.sprintf(
      "%s --print binary %s--parse re > %s",
      Commands.shellEscape(refmt),
      interface ? "-i true " : "",
      Commands.shellEscape(target),
    );
  /* Log.log("refmt " ++ moduleName ++ " " ++ cmd); */
  let (out, error, success) = Commands.execFull(~input=text, cmd);
  if (success) {
    /* Log.log("Worked on the first pass"); */
    Ok((None, target));
  } else {
    let goodError = Some(out @ error);
    let cmd =
      Printf.sprintf(
        "%s --print binary --recoverable --parse re > %s",
        Commands.shellEscape(refmt),
        Commands.shellEscape(target),
      );
    let (out, error, success) = Commands.execFull(~input=text, cmd);
    /* Log.log("Failed to refmt " ++ cmd ++ "\n" ++ String.concat("\n > ", out @ error)); */
    /* Log.log("The text:"); */
    /* Log.log(text); */
    if (!success) {
      /* Log.log("<< Failed to refmt " ++ cmd ++ "\n" ++ String.concat("\n > ", out @ error)); */
      Error(
        "Failed to refmt "
        ++ cmd
        ++ "\n"
        ++ String.concat("\n > ", out @ error),
      );
    } else {
      Ok((goodError, target));
    };
  };
};

let justBscCommand =
    (
      ~interface,
      ~reasonFormat,
      ~command,
      compilerPath,
      sourceFile,
      includes,
      flags,
    ) => {
  // TestRes
  let res =
    Filename.check_suffix(sourceFile, ".res")
    || Filename.check_suffix(sourceFile, ".resi");
  let flags =
    reasonFormat
      /* Filter out -pp flags for Reason files. Refmt is the only preprocessor
         that should apply to Reason files. */
      ? {
        let parts = Utils.split_on_char(' ', flags);
        let rec loop = items => {
          switch (items) {
          | ["-pp", _ppFlag, ...rest] => loop(rest)
          | [x, ...rest] => [x, ...loop(rest)]
          | [] => []
          };
        };
        loop(parts) |> String.concat(" ");
      }
      : flags;
  /* TODO make sure that bsc supports -color */
  Printf.sprintf(
    {|%s %s %s -bin-annot %s %s %s|},
    compilerPath,
    command,
    includes
    |> List.map(i => Printf.sprintf("-I %s", Commands.shellEscape(i)))
    |> String.concat(" "),
    flags ++ (reasonFormat ? " -bs-re-out" : ""),
    res ? "" : interface ? "-intf" : "-impl",
    sourceFile,
  );
};

let runBsc =
    (
      ~rootPath,
      ~interface,
      ~reasonFormat,
      ~command,
      compilerPath,
      sourceFile,
      includes,
      flags,
    ) => {
  let cmd =
    justBscCommand(
      ~interface,
      ~reasonFormat,
      ~command,
      compilerPath,
      sourceFile,
      includes,
      flags,
    );
  Log.log({|➡️ running compiler |} ++ cmd ++ " with pwd " ++ rootPath);
  let (out, error, success) = Commands.execFull(~pwd=rootPath, cmd);
  if (success) {
    Ok((out, error));
  } else {
    Error(out @ error);
  };
};

let cmtPath = (~cacheLocation, ~moduleName, ~uri) => {
  let interface = Utils.endsWith(uri, "i");
  cacheLocation /+ moduleName ++ ".cmt" ++ (interface ? "i" : "");
};

let getParsetree = (~cacheLocation, ~moduleName, ~uri) => {
  let cmt = cmtPath(~cacheLocation, ~moduleName, ~uri);
  Process_406.astForCmt(cmt);
};

let process =
    (
      ~uri,
      ~moduleName,
      ~rootPath,
      ~reasonFormat,
      text,
      ~cacheLocation,
      ~allLocations,
      compilerPath,
      refmtPath,
      includes,
      flags,
    ) => {
  let interface = Utils.endsWith(uri, "i");
  // TestRes
  let res = Utils.endsWith(uri, "res") || Utils.endsWith(uri, "resi");
  let%try (syntaxError, astFile) =
    switch (refmtPath) {
    | _ when res =>
      let astFile =
        cacheLocation /+ moduleName ++ ".res" ++ (interface ? "i" : "");
      let%try () = Files.writeFileResult(astFile, text);
      Ok((None, astFile));
    | Some(refmtPath) =>
      runRefmt(~interface, ~moduleName, ~cacheLocation, text, refmtPath)
    | None =>
      let astFile =
        cacheLocation /+ moduleName ++ ".ast" ++ (interface ? "i" : "");
      let%try () = Files.writeFileResult(astFile, text);
      Ok((None, astFile));
    };
  let fullForCmt = Process_406.fullForCmt(~moduleName, ~allLocations);
  let cmtPath =
    cacheLocation /+ moduleName ++ ".cmt" ++ (interface ? "i" : "");
  try(Unix.unlink(cmtPath)) {
  | _ => ()
  };
  switch (
    runBsc(
      ~rootPath,
      ~interface,
      ~reasonFormat,
      ~command="-c",
      compilerPath,
      astFile,
      includes,
      flags,
    )
  ) {
  | Error(lines) =>
    if (!Files.isFile(cmtPath)) {
      Ok(
        TypeError(
          String.concat("\n", lines),
          SharedTypes.initFull(moduleName, uri),
        ),
      );
    } else {
      Log.log("Now loading " ++ cmtPath);
      switch (Files.maybeStat(cmtPath)) {
      | Some({Unix.st_size: size}) =>
        Log.log("Size " ++ string_of_int(size))
      | _ => Log.log("Doesn't exist")
      };
      let%try_wrap {file, extra} = fullForCmt(cmtPath, uri, x => x);
      let errorText = String.concat("\n", lines);
      switch (syntaxError) {
      | Some(s) =>
        /** TODO also report the type errors / warnings from the partial result */
        SyntaxError(String.concat("\n", s), errorText, {file, extra})
      | None =>
        let errorText =
          switch (ErrorParser.parseDependencyError(errorText)) {
          | Some((name, _oname, _iface)) =>
            errorText
            ++ "\n\nThis is likely due to an error in module "
            ++ name
          | None => errorText
          };
        TypeError(errorText, {file, extra});
      };
    }
  | Ok((lines, error)) =>
    // Log.log("Now loading " ++ cmtPath);
    // switch (Files.maybeStat(cmtPath)) {
    // | Some({Unix.st_size: size}) =>
    // Log.log("Size " ++ string_of_int(size))
    // let ic = open_in_bin(cmtPath);
    // let buffer = really_input_string(ic,String.length(Config.cmi_magic_number));
    // Log.log(buffer);
    // Log.log(Config.cmi_magic_number);
    // close_in(ic);
    // | _ => Log.log("Doesn't exist")
    // };
    let%try_wrap full = fullForCmt(cmtPath, uri, x => x);
    Success(String.concat("\n", (res ? [] : lines) @ error), full);
  };
};
