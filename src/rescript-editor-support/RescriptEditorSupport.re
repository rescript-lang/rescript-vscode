open Infix;
module J = JsonShort;
module StringSet = Set.Make(String);
let capabilities =
  J.o([
    ("textDocumentSync", J.i(1)),
    ("hoverProvider", J.t),
    (
      "completionProvider",
      J.o([
        (
          "triggerCharacters",
          J.l([J.s("."), J.s(">"), J.s("@"), J.s("~")]),
        ),
      ]),
    ),
    ("definitionProvider", J.t),
    ("typeDefinitionProvider", J.t),
    ("referencesProvider", J.t),
    ("documentSymbolProvider", J.t),
    ("codeLensProvider", J.o([("resolveProvider", J.t)])),
    ("documentHighlightProvider", J.t),
    ("renameProvider", J.t),
  ]);

let getInitialState = params => {
  let rootUri = Json.get("rootUri", params) |?> Json.string |?> Uri2.parse;
  let%try rootUri = rootUri |> RResult.orError("Not a uri");
  let rootPath = Uri2.toPath(rootUri);

  Files.mkdirp(rootPath /+ "node_modules" /+ ".lsp");
  Log.setLocation(rootPath /+ "node_modules" /+ ".lsp" /+ "debug.log");
  Log.log("Hello - from " ++ Sys.executable_name);

  Rpc.sendNotification(
    stdout,
    "client/registerCapability",
    J.o([
      (
        "registrations",
        J.l([
          J.o([
            ("id", J.s("watching")),
            ("method", J.s("workspace/didChangeWatchedFiles")),
            (
              "registerOptions",
              J.o([
                (
                  "watchers",
                  J.l([
                    J.o([("globPattern", J.s("**/bsconfig.json"))]),
                    J.o([("globPattern", J.s("**/.merlin"))]),
                  ]),
                ),
              ]),
            ),
          ]),
        ]),
      ),
    ]),
  );

  let state = TopTypes.empty(~rootUri);

  Ok(state);
};

let parseArgs = args => {
  switch (args) {
  | [] => assert(false)
  | [_, ...args] =>
    let (opts, pos) =
      args
      |> List.rev
      |> List.fold_left(
           ((set, pos), arg) =>
             if (arg != "" && arg.[0] == '-') {
               (set |> StringSet.add(arg), pos);
             } else {
               (set, [arg, ...pos]);
             },
           (StringSet.empty, []),
         );
    (opts, pos);
  };
};

let hasOpt = (opts, name) => opts |> StringSet.mem(name);

let hasOpts = (opts, names) => names |> List.exists(opts |> hasOpt);

let hasVerbose = opts => hasOpts(opts, ["-v", "--verbose"]);

let help = {|
Commands for Rescript Language Server

-dump: compute definition and hover for Foo.res at line 0 and column 4:

rescript-editor-support.exe dump src/Foo.res:0:4

-complete: compute autocomplete for Foo.res at line 0 and column 4,
 where Foo.res is being edited and the editor content is in file current.res.

rescript-editor-support.exe complete src/Foo.res:0:4 current.res

The dump command can also omit `:line:column`, to show results for every position in the file. Several files can be specified on the command line.
|};

let showHelp = () => {
  prerr_endline(help);
};

let main = () => {
  switch (parseArgs(Sys.argv |> Array.to_list)) {
  | (opts, _) when hasOpts(opts, ["-h", "--help"]) => showHelp()
  | (opts, []) =>
    if (opts |> hasVerbose) {
      Log.spamError := true;
      References.debugReferences := true;
      MerlinFile.debug := true;
    };
    Log.log("Booting up");
    BasicServer.run(
      ~messageHandlers=MessageHandlers.handlers,
      ~notificationHandlers=NotificationHandlers.notificationHandlers,
      ~capabilities=_params => capabilities,
      ~getInitialState,
    );
    Log.log("Finished");
    Log.out^ |?< close_out;
  | (_opts, ["dump", ...files]) => EditorSupportCommands.dump(files)
  | (_opts, ["complete", pathWithPos, currentFile]) =>
    EditorSupportCommands.complete(~pathWithPos, ~currentFile)
  | _ =>
    showHelp();
    exit(1);
  };
};

main();
