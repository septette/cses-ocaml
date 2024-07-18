open Core


(*
keep a set of the available (row, col) coords
pass this around, do regular backtracking but first checking existence in the set   
*)
type cell = {row: int; col: int} [@@deriving sexp]

module Cell = struct
  type t = cell [@@deriving sexp]
  let compare = (fun c1 c2 ->
    let cmp = Int.compare c1.row c2.row in
    if cmp = 0 then Int.compare c1.col c2.col else cmp
  )
end

module CellSet = Set.Make(Cell)

(*
other state variables are the bitset (or regular set) of diag, etc.
could just compute them using the existing state but would be a bit messy.
use a mutable? record type for storing these
*)

module Board_state = struct
  type t = {
    rows: int;
    cols: int;
    diags: int;
    antidiags: int;
  }

  let create_empty = {rows = 0; cols = 0; diags = 0; antidiags = 0}
  let check_ok (t: t) ~row ~col =
    t.rows land (1 lsl row) = 0 &&
    t.cols land (1 lsl col) = 0 &&
    t.diags land (1 lsl (row + col)) = 0 &&
    t.antidiags land (1 lsl (8 + row - col)) = 0 

  let add (t: t) ~row ~col = {
    rows = t.rows lor (1 lsl row);
    cols = t.cols lor (1 lsl col);
    diags = t.diags lor (1 lsl (row + col));
    antidiags = t.antidiags lor (1 lsl (8 + row - col))
  }
end

(*
i/o
todo - find a better way to do this
*)

let read_board () =
  let update_board cell_char ~curr_set ~row ~col = 
    match cell_char with
    | '.' -> Set.add curr_set {row; col}
    | '*' -> curr_set
    | _ -> raise Exit
  in
  let rec read_line curr_row (curr_set: CellSet.t) =
    match curr_row with
    | 9 -> curr_set
    | _ -> (
      let curr_set = 
          match In_channel.input_line In_channel.stdin with
          | Some line -> line |>
                        String.to_sequence |>
                        Sequence.foldi ~f:(fun i acc ele -> update_board ele ~curr_set:acc ~row:curr_row ~col:(i+1) ) ~init:curr_set
          | None -> raise Exit
      in read_line (curr_row + 1) curr_set
    )
  in read_line 1 CellSet.empty

let solve board_set =
  let rec helper ~board_state ~row =
    let col_coords = List.init 8 ~f:(fun x -> x + 1) in
    match row with
    | 9 -> 1
    (* for 1->8, sum the result of dfsing into them *)
    | _ -> List.fold col_coords ~init:0 ~f:(
      fun acc col ->
        if Board_state.check_ok board_state ~row:row ~col:col &&
           Set.mem board_set {row; col} 
        then acc + (helper
                    ~board_state:(Board_state.add board_state ~row:row ~col:col)
                    ~row:(row+1))
        else acc
    )
  in helper ~board_state:Board_state.create_empty ~row:1

let () =
  let board = read_board () in
  printf "%d\n" (solve board)