open Cmdliner

let version = Version.version

module Docgen = struct
  let run file =
    let () =
      match Sys.getenv_opt "FROM_COMPILER" with
      | Some "true" -> Analysis.Cfg.isDocGenFromCompiler := true
      | _ -> ()
    in
    match Tools.extractDocs ~entryPointFile:file ~debug:false with
    | Ok s -> `Ok (Printf.printf "%s\n" s)
    | Error e -> `Error (true, e)

  let docgen_file =
    let env =
      let doc =
        "Internal usage: `true` to generate documentation from \
         rescript-compiler repo: \
         https://github.com/rescript-lang/rescript-compiler"
      in
      Cmd.Env.info "FROM_COMPILER" ~doc
    in
    let doc = "Path to ReScript file" in
    Arg.(required & (pos 0) (some string) None & info [] ~doc ~env ~docv:"PATH")

  let cmd =
    let doc = "Generate JSON Documentation. Output to standard output" in
    let info = Cmd.info "doc" ~version ~doc in
    Cmd.v info Term.(ret (const run $ docgen_file))
end

module Reanalyze = struct
  type args = {
    dce: bool;
    termination: bool;
    exception_: bool;
    ci: bool;
    config: bool;
    debug: bool;
    exclude_paths: string list option;
    experimental: bool;
    externals: bool;
    json: bool;
    live_names: string list option;
    live_paths: string list option;
    cmt_path: string option;
    write: bool;
    suppress: string list option;
    unsuppress: string list option;
  }

  let run
      {
        dce;
        termination;
        exception_;
        ci;
        config;
        debug;
        exclude_paths;
        experimental;
        externals;
        json;
        live_names;
        live_paths;
        cmt_path;
        write;
        suppress;
        unsuppress;
      } =
    let open Reanalyze in
    if dce then RunConfig.dce ();
    if termination then RunConfig.termination ();
    if exception_ then RunConfig.exception_ ();

    (* Enable all analysis if dce, termination and exception_ is false *)
    if (not dce) && (not termination) && not exception_ then RunConfig.all ();

    let open Common in
    Cli.debug := debug;
    Cli.ci := ci;
    Cli.experimental := experimental;
    Cli.json := json;
    Cli.write := write;
    Cli.liveNames := live_names |> Option.value ~default:[];
    Cli.livePaths := live_paths |> Option.value ~default:[];
    Cli.excludePaths := exclude_paths |> Option.value ~default:[];
    runConfig.unsuppress <- unsuppress |> Option.value ~default:[];
    runConfig.suppress <- suppress |> Option.value ~default:[];

    DeadCommon.Config.analyzeExternals := externals;

    if config then Paths.Config.processBsconfig ();

    runAnalysisAndReport ~cmtRoot:cmt_path

  let cmd =
    let doc =
      "Experimental analyses for ReScript and OCaml: globally dead \
       values/types, exception analysis, and termination analysis."
    in
    let version = Reanalyze.Version.version in
    let man =
      [
        `S Manpage.s_description;
        `P
          "Reanalyze will report all kind of analysis, dead code, exception \
           and termination";
        `S Manpage.s_examples;
        `I
          ( "rescript-tools reanalyze",
            "Report all analysis (dead code, exception and termination)" );
        `I ("rescript-tools reanalyze --dce", "Report only dead code");
      ]
    in
    let info = Cmd.info "reanalyze" ~version ~doc ~man in

    let exception_ =
      let doc =
        "Experimental exception analysis. The exception analysis is designed \
         to keep track statically of the exceptions that might be raised at \
         runtime. It works by issuing warnings and recognizing annotations"
      in
      Arg.(value & flag & info ["exception"] ~doc)
    in

    let termination =
      let doc = "Experimental termination analysis" in
      Arg.(value & flag & info ["termination"] ~doc)
    in

    let dce =
      let doc =
        "Enable experimental DCE. The dead code analysis reports on globally \
         dead values, redundant optional arguments, dead modules, dead types \
         (records and variants)."
      in
      Arg.(value & flag & info ["dce"] ~doc)
    in

    let ci =
      let doc = "Internal flag for use in CI" in
      Arg.(value & flag & info ["ci"] ~doc)
    in

    let config =
      let doc = "Read the analysis mode from rescript.json or bsconfig.json" in
      Arg.(value & flag & info ["config"] ~doc)
    in

    let debug =
      let doc = "Print debug information" in
      Arg.(value & flag & info ["debug"] ~doc)
    in

    let exclude_paths =
      let doc =
        "Exclude from analysis files whose path has a prefix in the list"
      in
      Arg.(
        value
        & opt (some (list ~sep:',' string)) None
        & info ["exclude-paths"] ~doc ~docv:"PATHS")
    in

    let experimental =
      let doc =
        "Turn on experimental analyses. This option is currently unused"
      in
      Arg.(value & flag & info ["experimental"] ~doc)
    in

    let externals =
      let doc = "Report on externals in dead code analysis" in
      Arg.(value & flag & info ["externals"] ~doc)
    in

    let json =
      let doc = "Print reports in JSON Format" in
      Arg.(value & flag & info ["json"] ~doc)
    in

    let live_names =
      let doc =
        "Consider all values with the given name as live. This automatically \
         annotates @live all the items in list"
      in
      Arg.(
        value
        & opt (some (list ~sep:',' string)) None
        & info ["live-names"] ~doc ~docv:"NAMES")
    in

    let live_paths =
      let doc =
        "Consider all values whose path has a prefix in the list as live. This \
         automatically annotates @live all the items on list"
      in
      Arg.(
        value
        & opt (some (list ~sep:',' string)) None
        & info ["live-paths"] ~doc ~docv:"NAMES")
    in

    let unsuppress =
      let doc =
        "Report on files whose path has a prefix in the list. overriding \
         --suppress (no-op if --suppress is not specified)\n\
        \         comma-separated-path-prefixes"
      in
      Arg.(
        value
        & opt (some (list ~sep:',' string)) None
        & info ["unsuppress"] ~doc ~docv:"PATHS")
    in

    let suppress =
      let doc =
        "Don't report on files whose path has a prefix in the list. \
         Comma-separated-path-prefixes"
      in
      Arg.(
        value
        & opt (some (list ~sep:',' string)) None
        & info ["suppress"] ~doc ~docv:"PATHS")
    in

    let cmt_path =
      let doc = "Path to .cmt files" in
      Arg.(value & opt (some string) None & info ["cmt-path"] ~doc ~docv:"PATH")
    in

    let write =
      let doc = "Write @dead annotations directly in the source files" in
      Arg.(value & flag & info ["write"] ~doc)
    in

    let parse dce termination exception_ externals live_names live_paths
        cmt_path exclude_paths suppress unsuppress experimental config ci write
        json debug =
      {
        dce;
        termination;
        exception_;
        ci;
        config;
        debug;
        exclude_paths;
        experimental;
        externals;
        json;
        live_names;
        live_paths;
        write;
        cmt_path;
        suppress;
        unsuppress;
      }
    in

    let cmd =
      Term.(
        const parse $ dce $ termination $ exception_ $ externals $ live_names
        $ live_paths $ cmt_path $ exclude_paths $ suppress $ unsuppress
        $ experimental $ config $ ci $ write $ json $ debug)
    in

    Cmd.v info Term.(const run $ cmd)
end

let cmd =
  let doc = "ReScript Tools" in
  let info = Cmd.info "rescript-tools" ~version ~doc in
  Cmd.group info [Docgen.cmd; Reanalyze.cmd]

let () = exit (Cmd.eval cmd)
