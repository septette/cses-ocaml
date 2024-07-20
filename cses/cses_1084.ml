open Core

let read_line () =
  match In_channel.input_line In_channel.stdin with
  | Some line -> line |> String.split ~on:' ' |> List.map ~f:Int.of_string
  | None -> raise Exit

let solve applicants apartments k =
  let rec helper applicants apartments acc =
    match (applicants, apartments) with
    | (_, []) -> acc
    | ([], _) -> acc
    | (person::rest_applicants, apartment::rest_apartments) ->
      if abs (person - apartment) <= k then
        helper rest_applicants rest_apartments (acc + 1)
      else if person < apartment then
        (* this person is too small for the apartment. but maybe someone else can use it *)
        helper rest_applicants apartments acc
      else
        (* this person is too big for the apartment. but maybe there's another apartment to match it *)
        helper applicants rest_apartments acc
  in 
  helper applicants apartments 0

let () =
  match read_line () with
  | [_; _; k] ->
    let applicants = read_line () |> List.sort ~compare:Int.compare in
    let apartments = read_line () |> List.sort ~compare:Int.compare in
    print_endline (Int.to_string (solve applicants apartments k))
  | _ -> raise Exit