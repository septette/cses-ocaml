open Core
open Io

let solve ints =
  let (best, _) =
  List.fold ints ~init:(Int.min_value, 0) ~f:(
    fun (best, curr) x ->
      let summed = curr + x in
      (max best summed, max summed 0)
  )
  in best

let () =
  let _ = Io.read_line () in
  let ints = Io.read_line () in
  print_endline ((solve ints) |> Int.to_string)