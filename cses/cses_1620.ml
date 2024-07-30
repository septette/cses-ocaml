open Core
open Io

let solve products machines =
  (* inclusive left, exclusive right time to make t products *)
  let doable time =
    let made = List.fold machines ~f:(fun acc x -> acc + (time / x)) ~init:0 in
    made >= products
  in
  let rec helper left right =
    if left >= right then left else
    let mid = left + ((right - left) / 2) in
    if doable mid then
      helper left mid
    else
      helper (mid + 1) right
  in
  (*
  todo - figure out how to use longs properly.
  these hacks are only because integer overflows and other imprecisions
  mean the wrong answer is given for some test cases
  so we need to pick a good right value
  *)
  let worst = (List.fold machines ~init:0 ~f:max) in
  let right = worst * products / (List.length machines) in
  helper 0 (right * 2)

let () =
  let args = Io.read_line () in
  match args with
  | [_; t_products] ->
    let machines = Io.read_line () in
    let ans = solve t_products machines in
    print_endline (Int.to_string ans)
  | _ -> raise Exit