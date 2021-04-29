let readFile ~filename =
  try
    (* windows can't use open_in *)
    let chan = open_in_bin filename in
    let content = really_input_string chan (in_channel_length chan) in
    close_in_noerr chan;
    Some content
  with _ -> None

let content = readFile "./randomfile.txt"

let () = match content with
| None -> raise Not_found
| Some content -> print_endline content
