open Core

(* Similar to a multiset in C++. *)
module Multiset = struct
  type ('k, 'cmp) t = ('k, int, 'cmp) Map.t
  let empty (comparator: ('k, 'cmp) Comparator.Module.t) : ('k, 'cmp) t =
    Map.empty comparator
  
  let add multiset element =
    Map.update multiset element ~f:(function
      | None -> 1
      | Some count -> count + 1
    )
  
  let remove multiset element = 
    Map.change multiset element ~f:(function
    | None -> None
    | Some count when count > 1 -> Some (count - 1)
    | Some _ -> None
  )

  type ('k) acceptable_comparator = 
    | KvComparator of (key:'k -> data:int -> 'k -> int)
    | KComparator of (key:'k -> target:'k -> int)

  let binary_search multiset element semantic comparator =
    match comparator with
    | KvComparator compare_kv ->
      Map.binary_search multiset semantic element ~compare: compare_kv
    | KComparator compare_k ->
      Map.binary_search multiset semantic element ~compare:(fun ~key ~data:_ target -> compare_k ~key ~target)
  
  let max_elt_exn multiset = 
    Map.max_elt_exn multiset
end

