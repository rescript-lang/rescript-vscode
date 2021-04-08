open Infix

let rec resolveNodeModulePath ~startPath name =
  let path = startPath /+ "node_modules" /+ name in
  match startPath with
  | "/" -> ( match Files.exists path with true -> Some path | false -> None )
  | _ -> (
    match Files.exists path with
    | true -> Some path
    | false ->
      resolveNodeModulePath ~startPath:(Filename.dirname startPath) name )
