open Core
open Io
open Option.Let_syntax

let solve ints =
  let ints = List.sort ~compare:Int.compare ints in
  let n = List.length ints in
  let ans =
    if n % 2 = 1 then
      List.nth ints (n / 2)
    else
      let%bind a1 = List.nth ints (n / 2) in
      let%bind a2 = List.nth ints ((n / 2) - 1) in
      Some ((a1 + a2) / 2)
  in
  match ans with
  | Some ans -> List.fold ints ~init:0 ~f:(fun acc x -> (x - ans) |> abs |> (+) acc )
  | None -> raise Exit

let () =
  let _ = Io.read_line () in
  let ints = Io.read_line () in
  print_endline ((solve ints) |> Int.to_string)