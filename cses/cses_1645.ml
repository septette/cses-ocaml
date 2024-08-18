open Core
open Io

let solve arr n =
  let ans = Array.create ~len:n 0 in
  let rec remove_indices_that_have_reached_smaller_element l i = 
    match l with
    | [] -> []
    | j::rest ->
      if arr.(j) > arr.(i) then (
        ans.(j) <- i + 1;
        remove_indices_that_have_reached_smaller_element rest i
      )
      else
        j::rest
  in
  (* use the accumulator to store the indices for which we have not yet found a smaller element *)
  let (_, _) = Array.fold_right arr ~init:([], (n-1)) ~f:(
    fun _ (acc, i) ->
      (* process until no more *)
      let acc = match acc with
      | [] -> [i]
      | some -> remove_indices_that_have_reached_smaller_element some i
      in
      (i :: acc, i - 1)
  )
  in 
  ans

  let () =
  let n = Io.read_int () in
  let arr = Io.read_line () |> Array.of_list in
  solve arr n |> Array.iter ~f:(printf "%d ")