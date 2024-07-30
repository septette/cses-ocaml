open Core
open Io

module Customer = struct
  type t = {
    i: int;
    arrive: int;
    depart: int;
    assigned: int
  } [@@deriving fields]
end

module Room = struct
  module T = struct
    type t = {
      available: int;
      i: int;
    } [@@deriving sexp_of, fields]

    let compare c1 c2 =
      let av = Int.compare c1.available c2.available in
      if not (phys_equal av 0) then av else Int.compare c1.i c2.i
  end
  include T
  include Comparator.Make(T)
end

let solve customers =
  let customers = List.mapi customers ~f:(
    fun i -> function
    | [arrive; depart] -> {Customer.
      i; arrive; depart; assigned = -1}
    | _ -> raise Exit
  ) in
  let customers = List.sort customers ~compare:(fun c1 c2 -> Int.compare c1.arrive c2.arrive) in
  let rec helper (custs: Customer.t list) latests acc =
    match custs with
    | [] -> acc
    | cust::rest ->
      let existing_room = Set.min_elt latests in
      (* todo - figure out how to remove this duplication *)
      let new_room = match existing_room with
      | Some room when (Room.available room) < cust.arrive -> room.i;
      | Some _ -> Set.length latests
      | None -> Set.length latests
      in
      let new_room_record = {Room.available=cust.depart; i=new_room} in
      let latests = match existing_room with
      | Some room when (Room.available room) < cust.arrive ->
        let latests = Set.remove latests room in
        Set.add latests new_room_record
      | Some _ -> Set.add latests new_room_record
      | None -> Set.add latests new_room_record
      in helper rest latests ({cust with assigned=(new_room+1)} :: acc)
    in
    let res = helper customers (Set.empty (module Room)) [] in
    res |> List.sort ~compare:(fun c1 c2 -> Int.compare c1.i c2.i) |> List.map ~f:Customer.assigned


let () =
  let n = Io.read_int () in
  let customers = List.init n ~f:(fun _ -> Io.read_line ()) in
  let ans = solve customers in
  List.fold ans ~init:0 ~f:max |> Int.to_string |> print_endline;
  List.iter ans ~f:(printf "%d ")
