open Core

let read_int () = 
  match In_channel.input_line In_channel.stdin with
  | Some line -> Int.of_string line
  | None -> raise Exit

let parse_input () = 
  read_int ()

let rec solve i n =
  match i with
  | i when i = (n + 1) -> ()
  | i ->
    let r2i = max 0 (i - 1) in
    let r3i = max 0 (i - 2) in
    (* 
      place first knight anywhere, then second knight anywhere else.
      then for a regular 2x3 grid, with first knight in top left, you can place second knight in 1 way to attack.
      you can do this 8 different ways (orientations).
      divide by 2 because two ways to place two knights.
    *)
    let ans = ( (i * i) * ((i * i) - 1) - (8 * (r2i * r3i)) ) / 2 in
    print_endline (string_of_int ans); solve (i + 1) n

let () =
  let n = parse_input () in
  solve 1 n