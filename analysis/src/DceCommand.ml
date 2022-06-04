let command ~path =
  Reanalyze.cli ();
  Reanalyze.RunConfig.dce ();
  Reanalyze.runAnalysis ~cmtRoot:None ~ppf:Format.std_formatter
