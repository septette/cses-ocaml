open Core
open Io

(*
unfortunately can't solve this purely functionally because
core deques are also mutable
so we use an array
*)

let solve songs =
  let rec helper ~left ~right ~best ~curr =
    if right = Array.length songs then best else
    let song = songs.(right) in
    if Set.mem curr song then
      helper ~left:(left + 1) ~right:right ~best:best ~curr:(Set.remove curr songs.(left))
    else
      helper ~left:left ~right:(right + 1) ~best:(max best ((Set.length curr) + 1)) ~curr:(Set.add curr song)
  in helper ~left:0 ~right:0 ~best:0 ~curr:Int.Set.empty

let () =
  let _ = Io.read_line () in
  let songs = Io.read_line () |> Array.of_list in
  print_endline ((solve songs) |> Int.to_string)