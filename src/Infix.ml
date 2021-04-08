(**
 * This combines a filter and a map.
 * You provide a function that turns an element into an optional of another element,
 * and you get a list of all of the present results.
 *)
let optMap : ('a -> 'b option) -> 'a list -> 'b list = fun fn items ->
  List.fold_left
    (fun result item ->
      match fn item with None -> result | Some res -> res :: result)
    [] items

let ( |! ) o d = match o with None -> failwith d | Some v -> v

let ( |? ) o d = match o with None -> d | Some v -> v

let ( |?? ) o d = match o with None -> d | Some v -> Some v

let ( |?> ) o fn = match o with None -> None | Some v -> fn v

let ( |?>> ) o fn = match o with None -> None | Some v -> Some (fn v)

let fold o d f = match o with None -> d | Some v -> f v

let logIfAbsent message x =
  match x with
  | None ->
    Log.log message;
    None
  | _ -> x

let ( /+ ) = Files.fileConcat
