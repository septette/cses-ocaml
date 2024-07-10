open Core

let read_int () = 
  match In_channel.input_line In_channel.stdin with
  | Some line -> Int.of_string line
  | None -> raise Exit

let read_line () =
  match In_channel.input_line In_channel.stdin with
  | Some line -> line
  | None -> raise Exit

let read_int_line () =
  read_line () |> String.split ~on:' ' |> List.map ~f:Int.of_string

let rec solve ints prev moves =
  match ints with
  | [] -> moves
  | x :: rest -> 
    let new_prev = max prev x in
    let additional_move = max 0 (prev - x) in
    solve rest new_prev (moves + additional_move)

let () =
  let _ = read_int () in
  let ints = read_int_line () in
  print_string (string_of_int (solve ints (-1) 0))

