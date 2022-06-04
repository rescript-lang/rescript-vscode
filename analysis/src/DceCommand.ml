let command () =
  Reanalyze.cli ();
  Reanalyze.RunConfig.dce ();
  let shouldRun = false in
  if shouldRun then (
    Reanalyze.runAnalysis ~cmtRoot:None;
    let issues = Reanalyze.Log_.Stats.getSortedIssues () in
    Printf.printf "kinds of issues:%d\n" (List.length (fst issues)))
