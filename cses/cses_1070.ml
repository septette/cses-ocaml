open Core

(*
Put the middle value at the start (every second place)
1..5

3  1 4 2  5

3 1 4 2

4 1 5 2 6 3

3 1 4 2

*)

let read_int () = 
  match In_channel.input_line In_channel.stdin with
  | Some line -> Int.of_string line
  | None -> raise Exit

let solve n =
  let rec helper acc i next_odd next_even =
    match i with
    | i when i = n -> acc
    | i when (i % 2) = 1 -> helper (next_odd :: acc) (i + 1) (next_odd + 1) next_even
    | i -> helper (next_even :: acc) (i + 1) next_odd (next_even + 1)
  in
  List.rev (helper [] 0 1 ((n / 2) + 1))

let verify ans =
  let (_, result) =
    List.fold ans
      ~init:(-1, true)
      ~f:(fun (prev, ok) curr ->
          let new_ok = ok && abs (prev - curr) >= 2 in
          (curr, new_ok))
  in
  match result with
  | true -> Some ans 
  | false -> None

let () =
  let n = read_int () in
  let result = (solve n) |> verify in
  match result with
  | Some ans -> List.iter ~f:(printf "%d ") ans
  | None -> printf "NO SOLUTION"
