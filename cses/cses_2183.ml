open Core
open Io

let solve coins =
  let coins = List.sort coins ~compare:Int.compare in
  let rec helper coins last_produceable =
    match coins with
    | [] -> last_produceable + 1
    | coin::rest -> 
      if coin > (last_produceable + 1) then last_produceable + 1
      else helper rest (last_produceable + coin)
  in helper coins 0

let () =
  let _ = Io.read_line () in
  let coins = Io.read_line () in
  print_endline ((solve coins) |> Int.to_string)