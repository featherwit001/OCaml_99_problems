type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

(** <44> Construct Completely Balanced Binary Trees  *)
(* Cartesian product of left and right subtrees sets 
   eg. [t1; t2; t3] and [t4; t5] 
      get [ (t1, t4); (t1, t5); (t2, t4); (t2, t5); (t3, t4); (t3, t5);]
    optimization: acc is the same at one situation, avoid @ ot flattern*)
let combine_left_and_right_trees acc left right =
  let combine_rights_to_one_left acc l = 
    List.fold_left (fun acc r -> Node('x', l, r) :: acc) acc  right in
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

(* <44> memoization optimization *)
let memo_rec f =
  let h = Hashtbl.create 16 in
  let rec g x = 
    try Hashtbl.find h x
    with Not_found ->
      let res = f g x in
      Hashtbl.add h x res;
      res
  in g 

let cbal_trees_memo_wrap = 
  let bal_trees self n = 
    if n = 0 then [Empty]
    else
    if n mod 2 = 1 then
      let t =  self (n / 2) in
      combine_left_and_right_trees [] t t
    else
      let t1 = self (n / 2) in
      let t2 = self (n / 2 -1) in 
      let combine_t1_t2 = combine_left_and_right_trees [] t1 t2 in
      combine_left_and_right_trees combine_t1_t2 t2 t1
    in
  memo_rec bal_trees
  

(* <45>  Symmetric Binary Trees *)
let rec is_mirror l r = 
  match l, r with
  | Empty, Empty -> true
  | Empty, Node (_, _, _ ) | Node (_,_,_) , Empty -> false
  | Node (_, ll, lr), Node (_, rl, rr)  -> 
    is_mirror ll rr
    && 
    is_mirror lr rl 
let is_symmetric = function
  | Empty -> true
  | Node (_, l, r) -> is_mirror l r 


(* <46> Binary Search Trees (Dictionaries) *)
let rec insert_binary_serach_trees v = function
  | Empty -> Node (v, Empty, Empty)
  | Node(x, l, r) -> 
    if v < x then Node (x, insert_binary_serach_trees v l, r)
    else Node (x, l, insert_binary_serach_trees v r)

let rec construct_aux tree lst = 
  match lst with
  | [] -> tree
  | h :: t -> construct_aux (insert_binary_serach_trees h tree) t

let construct lst = construct_aux Empty lst



(* <47> sym_cbal_trees *)
let sym_cbal_trees n =
  n |> cbal_tree |> List.filter (fun t -> is_symmetric t)


(* <48> Construct Height-Balanced Binary Trees  *)
let left_and_right_trees_combinations acc llst rlst = 
  let one_left_tree_combined_with_right_trees acc l = 
    List.fold_left (fun acc r -> Node ('x', l, r) :: acc) acc rlst
  in 
    List.fold_left (fun acc l -> one_left_tree_combined_with_right_trees acc l) acc llst

let rec hbal_tree height =
  if height < 0 then []
  else 
  if height = 0 then [Empty]
  else 
    let t1 = hbal_tree (height - 1) in
    let t2 = hbal_tree (height - 2) in 
    let less1 = left_and_right_trees_combinations [] t2 t1 in
    let more1_and_less1 = left_and_right_trees_combinations less1 t1 t2 in
    let equal_more1_and_less1 = left_and_right_trees_combinations  more1_and_less1 t1 t1 in
    equal_more1_and_less1  


(* <48> memo_rec  *)
let memo_rec f =
  let h = Hashtbl.create 16 in
  let rec g x =
    if Hashtbl.mem h x then Hashtbl.find h x 
    else
      let res = f g x in
      Hashtbl.add h x res;
      res
  in
  g

let hbal_tree_memo = 
  let bal_trees_of_height self h =
    if h < 0 then []
    else
    if h = 0 then [Empty]
    else
      let t1 = self (h - 1) in
      let t2 = self (h - 2) in 
      let less1 = left_and_right_trees_combinations [] t2 t1 in
      let more1_and_less1 = left_and_right_trees_combinations less1 t1 t2 in
      let equal_more1_and_less1 = left_and_right_trees_combinations more1_and_less1 t1 t1 in
      equal_more1_and_less1 
    in memo_rec bal_trees_of_height

(** <49> Construct Height-Balanced Binary Trees With a Given Number of Nodes *)

(* <49>  min and max nodes in a balance binary tree wiht height [h]*)

(* [max_nodes h] is the maximum nodes in the balance bianry tree with height [h]*)
let max_nodes h = 1 lsl h - 1

(* [min_nodes h] is the minimum nodes in the balance bianry tree with height [h]*)
let rec min_nodes h = 
  if h <= 0 then 0
  else 
  if h = 1 then 1
  else
    let subtree1 = min_nodes (h - 1) in 
    let subtree2 = min_nodes (h - 2) in
    subtree1 + subtree2 + 1

let rec min_node_loops m0 m1 h =
  if h <= 0 then m0
  else
  if h = 1 then m1
  else 
    min_node_loops m1 (m0 + m1 + 1) (h - 1)

let min_nodes_optimization h =
   min_node_loops 0 1 h 

(* <49>  min and max heights in a balance binary tree wiht [n] nodes *)


let rec min_height n = 
  if n = 0 then 0
  else
  if n = 1 then 1
  else
  if n mod 2 = 1 then
    let t = min_height (n / 2) in
    1 + t
  else 
    let t1 = min_height (n / 2) in
    let t2 = min_height (n / 2 - 1) in
    1 + max t1 t2

(* [min_height n] is the minimum height in the balance bianry tree with [n] nodes
   to get the minimum height, the subtree must be stuffed of nodes as many as possible
   the max amount of nodes of a bal bin tree is n/2 n/2 or n/2 n/2-1*)
let  min_height_memo = 
  let min_height self n = 
    if n = 0 then 0
    else
    if n = 1 then 1
    else
    if n mod 2 = 1 then
      let t = self (n / 2) in
      1 + t
    else 
      let t1 = self (n / 2) in
      let t2 = self (n / 2 - 1) in
      1 + max t1 t2
  in memo_rec min_height

(* max node of height h is 2^h - 1 , i.e. n = 2^h - 1 
   then the min height of n node is h = log [2] (n + 1)*)
let min_height_formula n = int_of_float (ceil (log (float(n + 1)) /. log (2.))) 

(* [max_height n] is the maximum height in the balance bianry tree with [n] nodes *)
let rec max_height_aux h n = 
  if min_nodes_optimization h <= n then max_height_aux (h + 1) n else h - 1
(* why not <, because when equal =, return h - 1 is wrong 
   eg. n = 4, 
    min_nodes 0 1 2 3 4
              0 1 2 4 7
    if choose < , when h = 3, min_nodes is 4 
      the if-guard is false then return h - 1 = 2 wrong. 
   *)

let max_height n = max_height_aux 0 n


let rec max_height_loop h min_nodes_h min_nodes_hmius1 n =
  if min_nodes_h <= n  
  then max_height_loop (h + 1) (min_nodes_h + min_nodes_hmius1 + 1) (min_nodes_h) n
  else h - 1
let max_height_optimization n =
  max_height_loop 0 0 0 n
  

let rec from acc i j = if i <= j then from (j :: acc) i (j - 1) else acc

(* [range i j] is the int lst from i to j including i and j.
   Require: i <= j  *)
let range i j = from [] i j 

let rec bal_tree_height_nodes h n = 
  assert (min_nodes h <= n && n <= max_nodes h);
  if h = 0 then [Empty]
  else 
    let t3 = bal_sub_trees_height_nodes (h - 1) (h - 1) (n - 1)in 
    let t1 = bal_sub_trees_height_nodes (h - 1) (h - 2) (n - 1) in 
    let t2 = bal_sub_trees_height_nodes (h - 2) (h - 1) (n - 1) in 
    t1 @ t2 @ t3
and bal_sub_trees_height_nodes lh rh n =
    let min_ln = max (min_nodes lh) (n - max_nodes rh) in
    let max_ln = min (max_nodes lh) (n - min_nodes rh) in
    let n1_range = range min_ln max_ln in
    (* for each (n1, n2) *)
    List.fold_left (
      fun acc n1 ->
        let lt = bal_tree_height_nodes lh n1 in
        let rt = bal_tree_height_nodes rh (n - n1) in
        (* for ecah left sub-tree *)
        List.fold_left (fun acc l -> 
          (* combined with each right sub-tree *)
          List.fold_left (fun acc r -> Node ('x', l, r) :: acc ) acc rt
          )  acc lt 
      ) [] n1_range

let hbal_tree_nodes n = 
  let height_range = range (min_height n) (max_height n) in
  List.fold_left 
    (fun acc h -> List.rev_append (bal_tree_height_nodes h n) acc ) 
    [] height_range
 

(* symmetric optimaize the hbal_tree_nodes *)
let symmetric_bal_tree acc =
  List.fold_left (fun acc t -> 
    match t with Empty -> acc | Node(v, l, r) -> Node (v, r, l) :: acc) acc acc

let rec bal_tree_height_nodes_optim h n = 
  assert (min_nodes h <= n && n <= max_nodes h);
  if h = 0 then [Empty]
  else 
    let acc = bal_sub_trees_height_nodes_optim [] (h - 1) (h - 2) (n - 1) in 
    let acc = symmetric_bal_tree acc in 
    let acc = bal_sub_trees_height_nodes_optim acc (h - 1) (h - 1) (n - 1)in 
    acc
and bal_sub_trees_height_nodes_optim acc lh rh n =
    let min_ln = max (min_nodes lh) (n - max_nodes rh) in
    let max_ln = min (max_nodes lh) (n - min_nodes rh) in
    let n1_range = range min_ln max_ln in
    (* for each (n1, n2) *)
    List.fold_left (
      fun acc n1 ->
        let lt = bal_tree_height_nodes_optim lh n1 in
        let rt = bal_tree_height_nodes_optim rh (n - n1) in
        (* for ecah left sub-tree *)
        List.fold_left (fun acc l -> 
          (* combined with each right sub-tree *)
          List.fold_left (fun acc r -> Node ('x', l, r) :: acc ) acc rt
          )  acc lt 
      ) acc n1_range

let hbal_tree_nodes_optimaization n = 
  let height_range = range (min_height n) (max_height n) in
  List.fold_left 
    (fun acc h -> List.rev_append (bal_tree_height_nodes_optim h n) acc ) 
    [] height_range
                              

(* memoization optimaization *)


let hbal_tree_nodes_memo n = 
  let hashtbl1 = Hashtbl.create 16 in 
  let rec bal_tree_height_nodes_optim h n = 
    if Hashtbl.mem hashtbl1 (h,n) then Hashtbl.find hashtbl1 (h,n)
    else begin 
      assert (min_nodes h <= n && n <= max_nodes h);
      if h = 0 then [Empty]
      else 
        let t1 = bal_sub_trees_height_nodes_optim (h - 1) (h - 2) (n - 1) in 
        let t2 = bal_sub_trees_height_nodes_optim (h - 2) (h - 1) (n - 1) in 
        let t3 = bal_sub_trees_height_nodes_optim (h - 1) (h - 1) (n - 1)in
        let res =  List.rev_append (List.rev_append t1 t2) t3 in 
        Hashtbl.add hashtbl1 (h, n) res;
        res    
      end
  and bal_sub_trees_height_nodes_optim lh rh n =        
        let min_ln = max (min_nodes lh) (n - max_nodes rh) in
        let max_ln = min (max_nodes lh) (n - min_nodes rh) in
        let n1_range = range min_ln max_ln in
        (* for each (n1, n2) *)
        let res = (List.fold_left (
          fun acc n1 ->
            let lt = bal_tree_height_nodes_optim lh n1 in
            let rt = bal_tree_height_nodes_optim rh (n - n1) in
            (* for ecah left sub-tree *)
            List.fold_left (fun acc l -> 
              (* combined with each right sub-tree *)
              List.fold_left (fun acc r -> Node ('x', l, r) :: acc ) acc rt
              )  acc lt 
          ) [] n1_range) in
        res
  in 
  let height_range = range (min_height n) (max_height n) in
  List.fold_left 
    (fun acc h -> List.rev_append (bal_tree_height_nodes_optim h n) acc ) 
    [] height_range





