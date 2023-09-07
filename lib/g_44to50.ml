(** <44> Construct Completely Balanced Binary Trees  *)
type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

(* Cartesian product of left and right subtrees sets 
   eg. [t1; t2; t3] and [t4; t5] 
      get [ (t1, t4); (t1, t5); (t2, t4); (t2, t5); (t3, t4); (t3, t5);]
    optimization: acc is the same at one situation, avoid @ ot flattern*)
let combine_left_and_right_trees acc left right =
  let combine_rights_to_one_left acc l = 
    List.fold_left (fun acc r -> Node('x', l, r) ::acc) acc  right in
  List.fold_left (fun acc l -> combine_rights_to_one_left acc l ) acc left 

let rec cbal_tree n = 
  if n = 0 then [Empty]
  else
  if n mod 2 = 1 then 
    (* left and right is the same subtrees sets *)
    let t = cbal_tree (n / 2) in
    combine_left_and_right_trees [] t t 
  else
    (* the elts amount difference between left and right subtrees is 1
       but have two symmetric cases.*)
    let t1 = cbal_tree (n / 2) in
    let t2 = cbal_tree (n / 2 -1) in
    let balance_factor_is_1 = combine_left_and_right_trees [] t1 t2 in
      combine_left_and_right_trees balance_factor_is_1 t2 t1


