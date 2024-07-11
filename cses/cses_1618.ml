open Core

let read_int () = 
  match In_channel.input_line In_channel.stdin with
  | Some line -> Int.of_string line
  | None -> raise Exit

let parse_input () = 
  read_int ()

let log_ base num = (log num) /. (log base)

(*
this is determined entirely by the number of 2s and 5s. 
5s are the bounding factor, since 2s are more common.
there will be n // 5 multiples of 5, n // 25 multiples of 25, etc.
*)
let solve n =
  let rec helper pow_5 acc =
  ( match pow_5 with
  | 0 -> acc
  | _ as pow_5 ->
    let curr_pow = Int.pow 5 pow_5 in
    helper (pow_5 - 1) (acc + (n / curr_pow))
  )
  in helper (Int.of_float (log_ 5.0 (Float.of_int n))) 0

let () =
  let n = parse_input () in
  print_endline (string_of_int (solve n))