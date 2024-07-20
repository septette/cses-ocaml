open Core

let read_line () =
  match In_channel.input_line In_channel.stdin with
  | Some line -> line |> String.to_list |> Array.of_list
  | None -> raise Exit

let rows = 7
let cols = 7
let path_match_l = 48

type direction = Down | Up | Left | Right

(*
This solution is very slow (15s...) because I'm using immutable sets instead of mutable sets. 
todo - refactor
*)

let direction_to_char dir =
  match dir with
  | Down -> 'D'
  | Up -> 'U'
  | Left -> 'L'
  | Right -> 'R'

type cell = {row: int; col: int} [@@deriving sexp]

module Cell = struct
  type t = cell [@@deriving sexp]
  let compare = (fun c1 c2 ->
    let cmp = Int.compare c1.row c2.row in
    if cmp = 0 then Int.compare c1.col c2.col else cmp
  )
  let move = (fun cell dir ->
    let (row_delta, col_delta) = match dir with
    | Down -> (1, 0)
    | Up -> (-1, 0)
    | Left -> (0, -1)
    | Right -> (0, 1)
    in {
    row = cell.row + row_delta;
    col = cell.col + col_delta;
    })
  
  let is_valid_cell = (fun cell -> 0 <= cell.row && cell.row < rows && 0 <= cell.col && cell.col < cols)
end

module CellSet = Set.Make(Cell)

let solve path_match =
  let target_row = 6 in
  let target_col = 0 in
  let rec helper ~i ~cell ~used =
    if i = path_match_l then
      if cell.row = target_row && cell.col = target_col then 1
      else 0
    else
      let dirs = [Down; Up; Left; Right] in
      List.fold dirs ~init:0 ~f:(
        fun acc dir ->
          let new_cell = Cell.move cell dir in
          if not (Cell.is_valid_cell new_cell) then acc
          else if Set.mem used new_cell then acc
          else
            let next_cell = Cell.move new_cell dir in
            let beside_cell_1 = if (phys_equal dir Down || phys_equal dir Up) then Cell.move new_cell Left else Cell.move new_cell Up in
            let beside_cell_2 = if (phys_equal dir Down || phys_equal dir Up) then Cell.move new_cell Right else Cell.move new_cell Down in
            let obstructing = (not (Cell.is_valid_cell next_cell)) || (Set.mem used next_cell) in
            if obstructing &&
              (Cell.is_valid_cell beside_cell_1 && not (Set.mem used beside_cell_1)) &&
              (Cell.is_valid_cell beside_cell_2 && not (Set.mem used beside_cell_2))
            then acc
            else if Char.equal (direction_to_char dir) path_match.(i) || Char.equal path_match.(i) '?' then
              acc + (helper ~i:(i + 1) ~cell:new_cell ~used:(Set.add used new_cell))
            else acc
      )
  in
  let start_cell = {row = 0; col = 0} in
  helper ~i:0 ~cell:start_cell ~used:(Set.add CellSet.empty start_cell)

let () =
  let path_match = read_line () in
  solve path_match |> Int.to_string |> print_endline
