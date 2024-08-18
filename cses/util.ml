open Core

module Util : sig
  val decrement:  map:('a, int, 'b) Map.t -> key:'a -> ('a, int, 'b) Map.t
  val increment:  map:('a, int, 'b) Map.t -> key:'a -> ('a, int, 'b) Map.t
end = struct
  let change_map ~map ~key ~delta = 
    match Map.find map key with
    | Some v ->
      if v + delta > 0 then Map.set map ~key:key ~data:(v + delta)
      else Map.remove map key
    | None -> Map.set map ~key:key ~data:delta

  let decrement ~map ~key = change_map ~map ~key ~delta:(-1)
  let increment ~map ~key = change_map ~map ~key ~delta:(1)
end