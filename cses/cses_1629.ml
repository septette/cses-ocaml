open Core

type movie = {start: int; finish: int}
let compare_movie e1 e2 =
  let finish = Int.compare e1.finish e2.finish in
  if not (Int.equal finish 0) then finish else
    Int.compare e1.start e2.start

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

let solve movies =
  let movies = List.fold movies ~init:[] ~f:(fun acc x ->
    match x with
    | [start; finish] -> {start; finish} :: acc
    | _ -> raise Exit
  ) in
  let movies = List.sort movies ~compare:compare_movie in
  let (_, n) =
  List.fold movies ~init:(0, 0) ~f:(fun (curr_end, num) movie ->
    if curr_end <= movie.start then (movie.finish, (num + 1))
    else (curr_end, num)
  ) in n

let () =
  let n = read_int () in
  let movies = read_lines n in
  solve movies |> Int.to_string |> print_endline