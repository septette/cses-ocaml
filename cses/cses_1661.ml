open Core
open Io

let solve ints target =
  let existing = Hashtbl.create (module Int) in
  let rec helper ints curr acc =
    match ints with
    | [] -> acc
    | num::rest ->
      let curr = (curr + num) in
      (* curr - complement = target *)
      let complement = curr - target in 
      let count' = Hashtbl.find existing complement in
      Hashtbl.incr existing curr ~by:1;
      match count' with
      | Some count -> helper rest curr (acc + count)
      | None -> helper rest curr acc
  in Hashtbl.incr existing 0 ~by:1; helper ints 0 0

let () =
  match Io.read_line () with
  | [_; target] ->
    let ints = Io.read_line () in
    solve ints target |> Int.to_string |> print_endline
  | _ -> raise Exit