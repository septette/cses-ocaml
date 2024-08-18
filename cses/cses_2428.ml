open Core
open Io
open Util

let triangle n = n * (n + 1) / 2

let solve arr n k =
  let rec helper left right distinct total =
    if right = n then
      total + (triangle (right - left))
    else
      (* increment right *)
      let new_distinct = Util.increment ~map:distinct ~key:arr.(right) in
      (* maximal *)
      if Map.length new_distinct > k then
        let new_distinct = Util.decrement ~map:new_distinct ~key:arr.(left) in
        (* right will be incremented again in the next iteration *)
        let new_distinct = Util.decrement ~map:new_distinct ~key:arr.(right) in
        helper (left + 1) right new_distinct (total + (right - left))
      else
        helper left (right + 1) new_distinct total

  in helper 0 0 Int.Map.empty 0

  let () =
  let args = Io.read_line () in
  match args with
  | [n; k] -> (
    let arr = Io.read_line () |> Array.of_list in
    solve arr n k |> printf "%d\n"
  )
  | _ -> raise Exit