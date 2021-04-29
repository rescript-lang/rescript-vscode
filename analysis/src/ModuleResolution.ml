let rec resolveNodeModulePath ~startPath name =
  let path = Filename.concat (Filename.concat startPath "node_modules") name in
  if Files.exists path then Some path
  else if startPath = "/" then None
  else resolveNodeModulePath ~startPath:(Filename.dirname startPath) name
