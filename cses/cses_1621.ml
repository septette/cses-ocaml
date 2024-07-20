open Core

let read_int () =
  match In_channel.input_line In_channel.stdin with
  | Some line -> Int.of_string line
  | None -> raise Exit
  
let read_line () =
  match In_channel.input_line In_channel.stdin with
  | Some line -> line |> String.split ~on:' ' |> List.map ~f:Int.of_string
  | None -> raise Exit

let solve ints =
  let rec helper ints curr =
    match ints with
    | [] -> Set.length curr
    | i::rest -> helper rest (Set.add curr i)
  in 
  helper ints Int.Set.empty

let () =
  let _ = read_int () in
  let ints = read_line () in
  print_endline (Int.to_string (solve ints))