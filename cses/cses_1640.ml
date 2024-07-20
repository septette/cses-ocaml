open Core

let read_line () =
  match In_channel.input_line In_channel.stdin with
  | Some line -> line |> String.split ~on:' ' |> List.map ~f:Int.of_string
  | None -> raise Exit

let solve (ints: (int * int) array) target =
  let rec helper left right =
    if left >= right then None
    else if fst ints.(left) + fst ints.(right) = target then
      Some (snd ints.(left) + 1, snd ints.(right) + 1)
    else if fst ints.(left) + fst ints.(right) > target then
      helper left (right - 1)
    else
      helper (left + 1) right
  in helper 0 ((Array.length ints) - 1)

let compare_tuple (t10, t11) (t20, t21) =
  let first = Int.compare t10 t20 in
  if first = 0 then Int.compare t11 t21 else first

let () =
  let args = read_line () in
  match args with
  | [_; target] -> (
      let ints = read_line () |> List.mapi ~f:(fun i x -> (x, i)) |> List.sort ~compare:compare_tuple |> Array.of_list in
      match solve ints target with
      | Some (first, second) -> printf "%d %d\n" first second
      | None -> printf "IMPOSSIBLE\n"
    )
  | _ -> raise Exit