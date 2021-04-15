let findStars line =
  let l = String.length line in
  let rec loop i =
    if i >= l - 1 then None
    else if line.[i] = '*' && line.[i + 1] = ' ' then Some (i + 2)
    else if line.[i] <> ' ' then None
    else loop (i + 1)
  in
  loop 0

let combine one two =
  match (one, two) with
  | None, None -> None
  | Some a, None -> Some a
  | None, Some b -> Some b
  | Some a, Some b -> ( match a = b with true -> Some a | false -> Some 0 )

let trimFirst num string =
  let length = String.length string in
  match length > num with
  | true -> String.sub string num (length - num)
  | false -> ""

let cleanOffStars doc =
  let lines = Str.split (Str.regexp_string "\n") doc in
  let rec loop lines =
    match lines with
    | [] -> None
    | [one] -> (
      match String.trim one = "" with true -> None | false -> findStars one )
    | one :: rest -> (
      match String.trim one = "" with
      | true -> loop rest
      | false -> combine (findStars one) (loop rest) )
  in
  let num = loop lines in
  match num with
  | None | Some 0 -> doc
  | Some num -> (
    match lines with
    | [] | [_] -> doc
    | one :: rest ->
      (if findStars one <> None then trimFirst num one else String.trim one)
      ^ "\n"
      ^ String.concat "\n" (rest |> List.map (trimFirst num)) )
