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

(*
Similar to an ordered statistics tree in C++, or a SortedList in Python.
Allows similar operations to a multiset, but with the addition of nth and rank in logn.
Using AVL tree rebalancing, so immutable - must reassign after all operations.
*)
module OrderStatsTree = struct
  type 'a node =
  | Empty
  | Node of {
    left: 'a node;
    right: 'a node;
    key: 'a;  (* as a binary search tree, keys must be unique *)
    size: int;  (* size of self, and all subchildren *)
    number: int;  (* number of elements at this node *)
    factor: int  (* balance factor *)
  } [@@deriving compare, sexp]

  exception Internal_tree_failure

  let create = Empty

  let rec mem (tree: 'a node) (target: 'a) =
    match tree with
    | Empty -> false
    | Node n ->
      if n.key = target then true
      else if target < n.key then mem n.left target
      else mem n.right target

  let rec find (tree: 'a node) (target: 'a) =
    match tree with
    | Empty -> None
    | Node n ->
      if n.key = target then Some (Node(n))
      else if target < n.key then find n.left target
      else find n.right target 

  let is_leaf (tree: 'a node) =
    match tree with
    | Empty -> false
    (* todo - use real compare *)
    | Node {left; right; _} -> phys_equal left Empty && phys_equal right Empty

  let _increment_size tree element =
    let rec helper node =
      match node with
      | Empty -> Empty
      | Node n -> Node({n with
        size = n.size + 1;
        number = n.size + (if n.key = element then 1 else 0);
        left = (if element < n.key then helper n.left else n.left);
        right = (if n.key < element then helper n.right else n.right)
      })
    in helper tree
  
  let _rotate_left tree =
    ()

  let _rotate_right tree = 
    ()

  let _insert_node tree element =
    let new_leaf = Node({
      left = Empty; right = Empty;
      key = element;
      size = 1;
      number = 1;
      factor = 0;
    }) in
    let rec helper node =
      match node with
      | Empty -> new_leaf
      | Node n when element < n.key -> 
        (* todo - unfinished *)
        Node ({
          n with
          size = n.size + 1;
          left = helper n.left;
          factor = n.factor - 1;
        })
      | Node n when n.key < element -> 
        Node ({
          n with
          size = n.size + 1;
          right = helper n.right;
          factor = n.factor + 1;
        })
      | _ -> raise Internal_tree_failure
    in helper tree

  let add (tree: 'a node) (element: 'a) : 'a node =
    match find tree element with
    | Some _ -> _increment_size tree element
    | None -> _insert_node tree element

end
