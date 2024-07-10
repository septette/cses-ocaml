open Core

let read_line () =
  match In_channel.input_line In_channel.stdin with
  | Some line -> line
  | None -> raise Exit

let solve line =
  let (_, ans, _) =
    Sequence.fold line ~init:(0, 0, '*') ~f:(
      fun (curr, best, prev) letter ->
        match prev with
        | '*' -> (1, 1, letter)
        | _ when Char.equal prev letter -> (curr + 1, (max best (curr + 1)), letter)
        | _ -> (1, best, letter)
    )
  in ans

let () =
  let result = read_line () |> String.to_list |> Sequence.of_list |> solve in
  print_string (string_of_int result)

