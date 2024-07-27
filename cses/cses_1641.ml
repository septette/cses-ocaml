open Core
open Io

let solve ints target =
  (* for each int, check if you can do 2sum with the rest for the remainder of the target *)
  (* could make this faster by sorting, then skipping numbers if they're the same as the prev number *)
  let rec twosum ints existing target i = 
    match ints with
    | [] -> None
    | num::_ when Hashtbl.mem existing (target - num) ->
      Some (Hashtbl.find_exn existing (target-num), i)
    | num::rest ->
      Hashtbl.set existing ~key:num ~data:i;
      twosum rest existing target (i+1)
  in
  let rec threesum ints i =
    match ints with
    | [] -> None
    | num::rest -> 
      match twosum rest (Hashtbl.create (module Int)) (target - num) (i+1) with
      | Some (j, k) -> Some (i, j, k)
      | None -> threesum rest (i + 1)
  in
  threesum ints 1

let () =
  let args = Io.read_line () in
  match args with
  | [_; target] ->
    let ints = Io.read_line () in (
      match solve ints target with
      | Some (i, j, k) -> printf "%d %d %d\n" i j k
      | None -> print_endline "IMPOSSIBLE"
    )
  | _ -> raise Exit