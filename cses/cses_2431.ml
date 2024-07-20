open Core

let read_int () =
  match In_channel.input_line In_channel.stdin with
  | Some line -> Int.of_string line
  | None -> raise Exit

let solve pos =
  let rec helper ?n_digits curr_pos curr_digits =
    (* how many digits will be used by the next 10 ** i  range. e.g. 10 - 99 *)
    let upcoming_digits =  ( (Int.pow 10 curr_digits) - (Int.pow 10 (curr_digits - 1)) ) * curr_digits in
    match (n_digits, curr_pos) with
    | (Some d, _) ->
      let pos = pos - curr_pos in
      let num_from_curr_pos = pos / d in
      let pos_in_num = pos % d in
      let num = num_from_curr_pos + (Int.pow 10 (d - 1)) in
      (* get nth digit (from start of num) *)
      (* convert num to a list of digits *)
      let num_as_list = List.init d ~f:(fun i -> ( num / (Int.pow 10 (d - i - 1)) ) % 10 ) in
      List.nth num_as_list pos_in_num
    | (None, _) when (curr_pos + upcoming_digits) > pos -> helper ?n_digits:(Some curr_digits) curr_pos curr_digits
    | (None, _) -> helper ?n_digits:None (curr_pos + upcoming_digits) (curr_digits + 1)
  in helper ?n_digits:None 1 1

let () =
  let n = read_int () in
  for _ = 1 to n do
    let k = read_int () in
    let ans = solve k in
    match ans with
    | Some k -> k |> Int.to_string |> print_endline
    | None -> raise Exit
  done