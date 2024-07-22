open Core
open Io
open Custom_containers

(*
each traffic light splits the segment
keep track of currently placed lights and use binary search to find the next/prev light
in order to find out what to add/remove to the set of segments
*)

let solve lights streetlen =
  let rec helper lights segments placed acc = 
    match lights with
    | [] -> acc
    | light::rest ->
      let prev = Option.value_map (Set.binary_search placed `Last_strictly_less_than light ~compare:Int.compare) ~default:0 ~f:Fun.id in
      let next = Option.value_map (Set.binary_search placed `First_strictly_greater_than light ~compare:Int.compare) ~default:streetlen ~f:Fun.id in
      let segment = next - prev in
      let segments = Multiset.remove segments segment in
      let segments = Multiset.add segments (next - light) in
      let segments = Multiset.add segments (light - prev) in
      let placed = Set.add placed light in
      helper rest segments placed ((fst (Multiset.max_elt_exn segments)) :: acc)
  in
  let module IntComparator = Comparator.Make(Int) in
  let segments = Multiset.empty (module Int) in
  let ans = helper lights (Multiset.add segments streetlen) Int.Set.empty []
  in List.rev ans

let () =
  let args = Io.read_line () in
  match args with
  | [streetlen; _] ->
    let lights = Io.read_line () in
    solve lights streetlen |> List.iter ~f:(printf "%d ")
  | _ -> raise Exit