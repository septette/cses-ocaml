open Core
open Io

type 'a linked_list_node = Node of 'a | None

let solve ints =
  let rec helper ints ll =
    match ints with
    | [] -> Map.count ll ~f:(fun v -> phys_equal v None)
    | i::rest ->
      let new_ll = match Map.mem ll (i-1) with
      | true -> Map.set ll ~key:(i-1) ~data:(Node(i))
      | false -> ll
      in let new_ll = Map.set new_ll ~key:i ~data:None
      in helper rest new_ll
  in helper ints Int.Map.empty

let () =
  let _ = Io.read_line () in
  let ints = Io.read_line () in
  print_endline ((solve ints) |> Int.to_string)