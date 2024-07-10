open Core

type point = {row: int; col: int}

let read_int () = 
  match In_channel.input_line In_channel.stdin with
  | Some line -> Int.of_string line
  | None -> raise Exit

let read_point () =
  match In_channel.input_line In_channel.stdin with
  | Some line -> 
    let parts = String.split line ~on:' ' in
    (
      match parts with
      | [row_str; col_str] ->
        let row = Int.of_string row_str in
        let col = Int.of_string col_str in
        {row; col}
      | _ -> raise Exit
    )
  | None -> raise Exit

type state = Start | Point

let parse_input () = 
  let rec helper state i n points =
    if i = n then points else
    match state with
    | Start -> helper Point 0 (read_int ()) []
    | Point -> helper Point (i + 1) n ((read_point ()) :: points)
    in
  List.rev (helper Start 0 (-1) [])

let solve point =
  let upper_ring = max point.row point.col in
  (* inclusive *)
  let upper = Int.pow upper_ring 2 in
  (* exclusive *)
  let lower = Int.pow (upper_ring - 1) 2 in
  (* 1 if increasing left->right, else 0 *)
  let parity = upper_ring % 2 in
  (* horizontal *)
  let start = if parity = 1 then lower + 1 else upper in
  let step = if parity = 1 then 1 else -1 in
  let steps = (min point.col upper_ring) + (upper_ring - point.row) in
  start + (step * (steps - 1))


let () =
  let points = parse_input () in
  List.map ~f:solve points |> List.iter ~f:(printf "%d\n")