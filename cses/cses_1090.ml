open Core

let read_line () =
  match In_channel.input_line In_channel.stdin with
  | Some line -> line |> String.split ~on:' ' |> List.map ~f:Int.of_string
  | None -> raise Exit

let to_deque a_list =
  let deque = Deque.create () in
  List.iter a_list ~f:(fun x -> Deque.enqueue_back deque x);
  deque

let solve children max_weight =
  let rec helper children acc =
    match (Deque.dequeue_front children, Deque.dequeue_back children) with
    | (None, None) -> acc
    | (Some _, None) -> acc + 1
    | (None, Some _) -> acc + 1
    | (Some front, Some back) ->
      if front + back <= max_weight then
        helper children (acc + 1)
      else (
        Deque.enqueue_front children front;
        helper children (acc + 1)
      )
  in
  helper children 0

let () =
  match read_line () with
  | [_; max_weight] ->
    let children = read_line () |> List.sort ~compare:Int.compare |> to_deque in
    print_endline (Int.to_string (solve children max_weight))
  | _ -> raise Exit