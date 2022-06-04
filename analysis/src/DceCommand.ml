let command () =
  Reanalyze.cli ();
  Reanalyze.RunConfig.dce ();
  Reanalyze.runAnalysis ~cmtRoot:None;
  let issues = Reanalyze.Log_.Stats.getSortedIssues () in
  Printf.printf "kinds of issues:%d\n" (List.length (fst issues))
