open Core

let read_int () = 
  match In_channel.input_line In_channel.stdin with
  | Some line -> Int.of_string line
  | None -> raise Exit

let read_apples () =
  match In_channel.input_line In_channel.stdin with
  | Some line -> line |> String.split ~on:' ' |> List.map ~f:Int.of_string
  | None -> raise Exit

let solve apples =
  let total = List.fold ~init:0 ~f:(+) apples in
  let rec helper curr apples best_diff =
    match apples with
    | [] ->
      let rest = total - curr in
      min best_diff (abs (rest - curr))
    | apple::rest_apples ->
      min (helper (curr + apple) rest_apples best_diff) (helper curr rest_apples best_diff)
  in helper 0 apples Int.max_value

let () =
  let _ = read_int () in
  let apples = read_apples () in
  printf "%d\n" (solve apples)