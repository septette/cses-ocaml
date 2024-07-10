open Core

let read_int () = 
  match In_channel.input_line In_channel.stdin with
  | Some line -> Int.of_string(line)
  | None -> raise Exit

let read_int_array_line () =
  match In_channel.input_line In_channel.stdin with
  | Some line -> String.split line ~on:' ' |> List.map ~f:Int.of_string 
  | None -> raise Exit

let rec check_existence n set =
  match n, set with
  | 1, _ -> 1
  | n, set ->
    if Set.mem set n then check_existence (n-1) set
    else n

let () =
  let n = read_int () in
  let integers = read_int_array_line () in
  let int_set = Int.Set.of_list(integers) in
  let result = check_existence n int_set in
  print_string (string_of_int result)