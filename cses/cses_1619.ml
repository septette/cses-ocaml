open Core

type event_type = Arrival | Departure
type event = {time: int; event_type: event_type}
let compare_event e1 e2 = Int.compare e1.time e2.time
let event_to_delta = function
  | Arrival -> 1
  | Departure -> (-1)

let read_int () =
  match In_channel.input_line In_channel.stdin with
  | Some line -> line |> Int.of_string
  | None -> raise Exit

let read_line () =
  match In_channel.input_line In_channel.stdin with
  | Some line -> line |> String.split ~on:' ' |> List.map ~f:Int.of_string
  | None -> raise Exit

let read_lines n = 
  let rec helper n acc =
    match n with
    | 0 -> acc
    | _ -> helper (n - 1) ((read_line ()) :: acc)
  in helper n []

let solve people =
  let people = List.fold people ~init:[] ~f:(fun acc x ->
    match x with
    | [start; depart] -> {time = start; event_type = Arrival} :: {time = depart; event_type = Departure} :: acc
    | _ -> raise Exit
  ) in
  let people = List.sort people ~compare:compare_event in
  let (_, best) = List.fold people ~init:(0, 0) ~f:(
    fun (curr, best) event ->
      let curr = curr + event_to_delta event.event_type in
      (curr, max curr best)
  )
  in best

let () =
  let n = read_int () in
  let people = read_lines n in
  solve people |> Int.to_string |> print_endline