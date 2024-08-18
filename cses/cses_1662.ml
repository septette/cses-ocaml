open Core
open Io

let triangle n = 
  (n + 1) * n / 2

let solve arr n =
  (* keep track of indices where the sum is same, mod n *)
  let (parity, _) = List.foldi arr ~init:(Map.set Int.Map.empty ~key:0 ~data:[-1], 0) ~f:(
    fun i (acc, curr_sum) ele ->
      let curr_sum = (curr_sum + ele) mod n in
      (* ocaml is not the same as python, negative numbers mod n result in negative numbers *)
      let k = (curr_sum + n) mod n in
      let acc = match Map.find acc k with
      | Some l -> Map.set acc ~key:k ~data:(i :: l)
      | None -> Map.set acc ~key:k ~data:[i] in
      (acc, curr_sum)
  ) in
  Map.fold parity ~init:0 ~f:(
    fun ~key:_ ~data:d total -> 
      total + triangle ((List.length d) - 1)
  )

  let () =
  let n = Io.read_int () in
  let arr = Io.read_line () in
  solve arr n |> Int.to_string |> print_endline