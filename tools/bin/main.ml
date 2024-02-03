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
  type dir = Suppress of string list | Unsuppress of string list
  type analysis = All | DCE | Termination | Exception

  type args = {
    analysis: analysis;
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
    suppress_unsuppress: dir option;
  }

  let run
      {
        analysis;
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
        suppress_unsuppress;
      } =
    let open Reanalyze in
    (* Set kind of analysis *)
    (match analysis with
    | All -> RunConfig.all ()
    | DCE -> RunConfig.dce ()
    | Termination -> RunConfig.termination ()
    | Exception -> RunConfig.exception_ ());

    if config then Paths.Config.processBsconfig ();

    let () =
      let open Common in
      Cli.debug := debug;
      Cli.ci := ci;
      Cli.experimental := experimental;
      Cli.json := json;
      Cli.write := write;
      (Cli.liveNames :=
         match live_names with
         | None -> []
         | Some l -> l);
      (Cli.livePaths :=
         match live_paths with
         | None -> []
         | Some l -> l);
      Cli.excludePaths :=
        match exclude_paths with
        | None -> []
        | Some l -> l
    in

    DeadCommon.Config.analyzeExternals := externals;

    (match suppress_unsuppress with
    | Some kind -> (
      match kind with
      | Suppress dirs -> Common.runConfig.suppress <- dirs
      | Unsuppress dirs -> Common.runConfig.unsuppress <- dirs)
    | None -> ());

    runAnalysisAndReport ~cmtRoot:cmt_path

  let cmd =
    let doc =
      "Experimental analyses for ReScript and OCaml: globally dead \
       values/types, exception analysis, and termination analysis."
    in
    let version = Reanalyze.Version.version in
    let info = Cmd.info "reanalyze" ~version ~doc in

    let analysis =
      let all =
        let doc = "Run all the analyses: DCE, Exception and Termination" in
        (All, Arg.info ["all"] ~doc ~absent:"Reanalyze run all analysis")
      in
      let dce =
        let doc =
          "Enable experimental DCE. The dead code analysis reports on globally \
           dead values, redundant optional arguments, dead modules, dead types \
           (records and variants)."
        in
        (DCE, Arg.info ["dce"] ~doc)
      in
      let termination =
        let doc = "Experimental termination analysis" in
        (Termination, Arg.info ["termination"] ~doc)
      in
      let exception_ =
        let doc =
          "Experimental exception analysis. The exception analysis is designed \
           to keep track statically of the exceptions that might be raised at \
           runtime. It works by issuing warnings and recognizing annotations"
        in
        (Exception, Arg.info ["exception"] ~doc)
      in

      Arg.(last & vflag_all [All] [all; dce; termination; exception_])
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
        & info ["live-names"] ~doc ~docv:"PATHS")
    in

    let live_paths =
      let doc =
        "Consider all values whose path has a prefix in the list as live. This \
         automatically annotates @live all the items on list"
      in
      Arg.(
        value
        & opt (some (list ~sep:',' string)) None
        & info ["live-paths"] ~doc ~docv:"PATHS")
    in

    let suppress_unsuppress_exclusive =
      let open Cmdliner.Term in
      let exclusive_msg =
        "The options --suppress and --unsuppress are mutually exclusive."
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

      let unsuppress =
        let doc =
          "Report on files whose path has a prefix in the list. \
           comma-separated-path-prefixes"
        in
        Arg.(
          value
          & opt (some (list ~sep:',' string)) None
          & info ["unsuppress"] ~doc ~docv:"PATHS")
      in

      let suppress_unsuppress suppress unsuppress =
        match (suppress, unsuppress) with
        | None, None -> `Ok None
        | Some s, None -> `Ok (Some (Suppress s))
        | None, Some s -> `Ok (Some (Unsuppress s))
        | _ -> `Error (true, exclusive_msg)
      in
      ret (const suppress_unsuppress $ suppress $ unsuppress)
    in

    let cmt_path =
      let doc = "Path to .cmt files" in
      Arg.(value & opt (some string) None & info ["cmt-path"] ~doc ~docv:"PATH")
    in

    let write =
      let doc = "Write @dead annotations directly in the source files" in
      Arg.(value & flag & info ["write"] ~doc)
    in

    let parse analysis externals live_names live_paths cmt_path exclude_paths
        suppress_unsuppress experimental config ci write json debug =
      {
        analysis;
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
        suppress_unsuppress;
      }
    in

    let cmd =
      Term.(
        const parse $ analysis $ externals $ live_names $ live_paths $ cmt_path
        $ exclude_paths $ suppress_unsuppress_exclusive $ experimental $ config
        $ ci $ write $ json $ debug)
    in

    Cmd.v info Term.(const run $ cmd)
end

let cmd =
  let doc = "ReScript Tools" in
  let info = Cmd.info "rescript-tools" ~version ~doc in
  Cmd.group info [Docgen.cmd; Reanalyze.cmd]

let () = exit (Cmd.eval cmd)
