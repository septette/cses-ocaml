open Core

let read_line () = 
  match In_channel.input_line In_channel.stdin with
  | Some line -> line
  | None -> raise Exit

let counter seq =
  Sequence.fold
    ~init:Char.Map.empty
    ~f:(
      fun acc x ->
        let count = match Map.find acc x with
        | None -> 0
        | Some c -> c
      in 
      Map.set acc ~key:x ~data:(count + 1)
    )
    seq

let solve line =
  let count = counter (line |> String.to_sequence) in
  let evens = Map.filter ~f:(fun v -> v % 2 = 0) count in
  let odds = Map.filter ~f:(fun v -> v % 2 = 1) count in
  match odds with
  | _ as odds when Map.length odds > 1 -> "NO SOLUTION"
  | _ as odds ->
    let center =
      match Map.to_alist odds with
      | (key, value) :: _ -> String.make value key
      | [] -> ""
    in
    let buffer = Buffer.create (String.length line) in
    let evens_kv = Map.to_alist evens in
    List.iter evens_kv ~f:(fun (k, v) -> Buffer.add_string buffer (String.make (v / 2) k));
    Buffer.add_string buffer center;
    List.iter (List.rev evens_kv) ~f:(fun (k, v) -> Buffer.add_string buffer (String.make (v / 2) k));
    Buffer.contents buffer

let () =
  print_endline (solve (read_line ()))