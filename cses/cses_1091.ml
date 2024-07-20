open Core

let read_line () =
  match In_channel.input_line In_channel.stdin with
  | Some line -> line |> String.split ~on:' ' |> List.map ~f:Int.of_string
  | None -> raise Exit

let modify a_map item ~delta = 
  match Map.find a_map item with
  | Some v -> Map.set a_map ~key:item ~data:(v + delta)
  | None -> Map.set a_map ~key:item ~data:1

let increment a_map = modify a_map ~delta:1
let decrement a_map = modify a_map ~delta:(-1)

let to_counter a_list =
  let rec helper a_list acc = 
    match a_list with
    | [] -> acc
    | item::rest_items -> 
      helper rest_items (increment acc item)
    in helper a_list Int.Map.empty


let solve tickets people =
  let rec helper tickets people acc =
    match people with
    | [] -> acc
    | person::rest_people ->
      if Map.length tickets = 0 then
        helper tickets rest_people (-1 :: acc)
      else 
        match Map.closest_key tickets `Less_or_equal_to person with
        | Some (ticket, v) when v > 1 -> helper (decrement tickets ticket) rest_people (ticket :: acc)
        | Some (ticket, _) -> helper (Map.remove tickets ticket) rest_people (ticket :: acc)
        | None -> helper tickets rest_people (-1 :: acc)
  in
  helper tickets people []

let () =
  let _ = read_line () in
  let tickets = read_line () |> to_counter in
  let people = read_line () in
  let ans = solve tickets people in
  List.rev ans |> List.iter ~f:(printf "%d\n")