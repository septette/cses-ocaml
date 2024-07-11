open Core

type pile = {pile_1: int; pile_2: int}

let read_int () = 
  match In_channel.input_line In_channel.stdin with
  | Some line -> Int.of_string line
  | None -> raise Exit

let read_pile () = 
  match In_channel.input_line In_channel.stdin with
  | Some line ->
    let split = line |> String.split ~on:' ' |> List.map ~f:Int.of_string in (
     match split with
     | [pile_1; pile_2] -> {pile_1; pile_2}
     | _ -> raise Exit
    )
  | None -> raise Exit

let solve pile =
  if not (Int.equal ((pile.pile_1 + pile.pile_2) % 3) 0) then 
    false
  else
    let diff = abs (pile.pile_1 - pile.pile_2) in
    if diff > min pile.pile_1 pile.pile_2 then
    false
  else
    true

let () =
  let n = read_int () in
  for _ = 1 to n do
    let possible = solve (read_pile ()) in
    match possible with
    | true -> print_endline "YES"
    | false -> print_endline "NO"
  done