open Core

let read_line () = 
  match In_channel.input_line In_channel.stdin with
  | Some line -> line
  | None -> raise Exit

let factorial n =
  let rec helper i acc =
    match i with
    | 1 -> acc
    | _ -> helper (i - 1) (acc * i)
  in helper n 1

let create_counter () : (char, int) Hashtbl.t =
  Hashtbl.create (module Char)

let counter line =
  let count = create_counter () in
  Sequence.iter ~f:(fun x -> Hashtbl.incr count x) line;
  count

let get_ways count = 
  let sum_v = Hashtbl.fold ~init:0 ~f:(fun ~key:_ ~data:v acc -> acc + v) count in
  let total_ways = factorial sum_v in
  Hashtbl.fold ~init:total_ways ~f:(fun ~key:_ ~data:v acc -> acc / (factorial v)) count

let solve count n =
  let rec helper stack =
    if List.length stack = n then
      print_endline (String.concat (List.rev stack))
    else
      for code = Char.to_int 'a' to  Char.to_int 'z' do
        let char = Char.of_int code in
        match char with
        | None -> raise Exit
        | Some char ->
        match Hashtbl.find count char with
        | None -> ()
        | Some _ ->
          Hashtbl.decr count char ~remove_if_zero:true;
          helper (String.of_char char :: stack);
          Hashtbl.incr count char;
      done
  in helper []

let () =
  let chars = read_line () in
  let n = String.length chars in
  let count = chars |> String.to_sequence |> counter in
  printf "%d\n" (get_ways count);
  solve count n