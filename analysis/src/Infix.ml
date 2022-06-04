let ( |? ) o d = match o with None -> d | Some v -> v

let ( |?> ) o fn = match o with None -> None | Some v -> fn v

let ( |?>> ) o fn = match o with None -> None | Some v -> Some (fn v)
