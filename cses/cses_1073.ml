open Core
open Io

(*
greedily solve this by putting cubes on top of the largest tail that exists
if no tail exists make a new stack
use maps to approximate heaps again
*)


let change_map ~map ~key ~delta = 
  match Map.find map key with
  | Some v ->
    if v + delta > 0 then Map.set map ~key:key ~data:(v + delta)
    else Map.remove map key
  | None -> Map.set map ~key:key ~data:delta

let decrement ~map ~key = change_map ~map ~key ~delta:(-1)
let increment ~map ~key = change_map ~map ~key ~delta:(1)

let solve cubes =
  let final_stacks = List.fold cubes ~init:Int.Map.empty ~f:(
    fun acc cube ->
      let acc = 
        match Map.binary_search acc `First_strictly_greater_than cube
              ~compare:(fun ~key ~data:_ target -> Int.compare key target) with
        | Some (key, _) -> decrement ~map:acc ~key:key
        | None -> acc
      in increment ~map:acc ~key:cube 
  ) in
  Map.fold final_stacks ~init:0 ~f:(fun ~key:_ ~data -> (+) data)

let () =
  let _ = Io.read_line () in
  let cubes = Io.read_line () in
  print_endline ((solve cubes) |> Int.to_string)