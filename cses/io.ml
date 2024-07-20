open Core

module Io = struct
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
end