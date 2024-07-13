open Core

let read_int () = 
  match In_channel.input_line In_channel.stdin with
  | Some line -> Int.of_string line
  | None -> raise Exit

let solve n =
  let rec helper i =
    match i with
    | 0 -> []
    | 1 -> ["0"; "1"]
    | _ as i ->
      let subresult = helper (i-1) in
      List.map subresult ~f:(fun x -> "0" ^ x) @ List.map (List.rev subresult) ~f:(fun x -> "1" ^ x)
  in helper n

let () =
  let ans = solve (read_int ()) in
  List.iter ans ~f:print_endline