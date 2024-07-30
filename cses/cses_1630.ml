open Core
open Io

type task = {
  duration: int;
  deadline: int
}

let solve tasks =
  let tasks = List.map tasks ~f:(function [duration; deadline] -> {duration; deadline} | _ -> raise Exit) in
  let tasks = List.sort tasks ~compare:(fun t1 t2 -> Int.compare t1.duration t2.duration) in
  let (_, reward) = List.fold tasks ~init:(0, 0) ~f:(
    fun (curr_time, reward) task -> (curr_time + task.duration, reward + task.deadline - (curr_time + task.duration))
  )
  in
  reward

let () =
  let n = Io.read_int () in
  let tasks = List.init n ~f:(fun _ -> Io.read_line () ) in
  solve tasks |> Int.to_string |> print_endline