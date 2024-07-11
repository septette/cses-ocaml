open Core

let read_int () = 
  match In_channel.input_line In_channel.stdin with
  | Some line -> Int.of_string line
  | None -> raise Exit

let parse_input () = 
  read_int ()

let mod_val = (Int.pow 10 9) + 7

let rec solve n acc =
  match n with
  | 0 -> acc
  | _ as n -> solve (n - 1) ((acc * 2) % mod_val)

let () =
  let n = parse_input () in
  print_endline (string_of_int (solve n 1))
