open G_44to49

(** <50> Count the Leaves of a Binary Tree *)
let rec count_leaves = function
  | Empty -> 0
  | Node (_, Empty, Empty) -> 1 (* Leaves is Node with Empty as two subtrees*)
  | Node (_, l, r) -> 1 + count_leaves l + count_leaves r

(** <51>  Collect the Leaves of a Binary Tree in a List*)
let rec leaves_aux acc = function
  | Empty -> acc
  | Node (v, Empty, Empty) as l ->  l :: acc
  | Node (v, l, r) -> leaves_aux (leaves_aux acc l) r 

let leaves tree = leaves_aux [] tree


(** <52> Collect the Internal Nodes of a Binary Tree in a List *)
let rec internals_aux acc = function
  | Empty | Node (_, Empty, Empty) -> acc
  (* this case can be included in the third case, 
     but this is a correct usage of inducing multiple scenarios *)
  | Node (v, Empty, subtree) | Node (v, subtree, Empty) as n 
                        ->  internals_aux (n::acc) subtree
  | Node (v, l, r) as n ->  internals_aux (internals_aux (n :: acc)l) r

let internals lst = internals_aux [] lst 


(** <53> Collect the Nodes at a Given Level in a List *)

let rec at_level_aux acc cur_level t level = 
  match t with
  | Empty -> acc
  | Node (x, l, r) ->
      if cur_level = level then x :: acc
      else if cur_level < level then 
          at_level_aux (at_level_aux acc (cur_level + 1) l level) (cur_level + 1) r (level)
      else acc

let at_level t l = List.sort compare (at_level_aux [] 1 t l)



(** <54> Construct a Complete Binary Tree  *)
module CompleteBinaryTree = struct
  (* index form 1, 
     size means how many elts in the tree from 0 
     capacity means the array's length*)
  type 'a t = {
    tree : 'a option array;
    capacity : int;
    size : int  
  }

  let get_elt heap i =
    if i <= heap.size then heap.tree.(i)
    else None

  let get_parent (heap : 'a t ) (i : int ) =
    if i / 2 <> 0 then heap.tree.(i / 2) 
    else None

  let get_left_child  heap i =
    if i * 2 <= heap.size then  heap.tree.(i * 2)
    else None

  let get_right_child heap i = 
    if i * 2 + 1 <= heap.size then heap.tree.(i * 2 + 1)
    else None
    
  let of_list lst =
    let pre_tree =  Array.of_list lst in
    let len = Array.length pre_tree in 
    let tree = Array.init (len + 1) (fun i ->if i = 0 then None else Some pre_tree.(i - 1)) in
  {
    tree = tree;
    size = len;
    capacity = len;
  }
end


let rec construct_cbt cbt i =
  match CompleteBinaryTree.get_elt cbt i with
  | None -> Empty
  | Some x -> Node (x, 
                construct_cbt cbt (2 * i), 
                construct_cbt cbt (2 * i + 1))

let complete_binary_tree lst = 
  let cbt =  CompleteBinaryTree.of_list lst in
  construct_cbt cbt 1




(** <55> Layout a Binary Tree (1)  
    which is the same as the print_binary_tree that I have done.
    but the index form 1

      1 2 3 4 5 6 7 8 9 col
   1  
   2  
   3  
   4  
   5 
   6 
 depth
    *)

let example_layout_tree1 =
  let leaf x = Node (x, Empty, Empty) in
  Node ('n', Node ('k', Node ('c', leaf 'a',
                           Node ('h', Node ('g', leaf 'e', Empty), Empty)),
                 leaf 'm'),
       Node ('u', Node ('p', Empty, Node ('s', leaf 'q', Empty)), Empty))

let rec tree_span_easy depth base elt_to_string = function
  | Empty -> (depth, 0, 0, 0, Empty)
  | Node (x, l, r) ->
    let xs = elt_to_string x in
    let xspan = String.length xs in

    let (ldepth, llspan, lvspan, lrspan, ln) = 
      tree_span_easy (depth + 1) base elt_to_string l in

    let left_subtree_span = llspan + lvspan + lrspan in
    let right_sub_tree_base = base + left_subtree_span + xspan in 

    let (rdepth, rlspan, rvspan, rrspan, rn) = 
      tree_span_easy (depth + 1) right_sub_tree_base elt_to_string r in

    let right_subtree_span = rlspan + rvspan + rrspan in  

    (depth, left_subtree_span, xspan, right_subtree_span, 
    Node((x, base + left_subtree_span, depth),ln, rn) )  
(* common mistake root base =  base + left_subtree_span *)

let layout_binary_tree_1 tree =
  let (_, _, _, _, n) = tree_span_easy 1 1 Char.escaped tree in n 

let layout_binary_tree_1_ref t =
  let rec layout depth x_left to_string = function
    | Empty -> (Empty, x_left)
    | Node (x, l, r) ->
      let xs = to_string x in
      let xspan = String.length xs in
      let (l', l_x_max) = layout (depth + 1) x_left to_string l in
      let (r', r_x_max) = layout (depth + 1) (l_x_max + xspan) to_string r in 
     (Node ((x, l_x_max ,depth), l', r'), r_x_max)
    in 
  fst (layout 1 1 Char.escaped t)


(** <56>  layout_binary_tree_2   *)
let example_layout_tree2 =
  let leaf x = Node (x, Empty, Empty) in
  Node ('n', Node ('k', Node ('c', leaf 'a',
                           Node ('e', leaf 'd', leaf 'g')),
                 leaf 'm'),
       Node ('u', Node ('p', Empty, leaf 'q'), Empty))
                
(** Require: the node sizes are One, like 'a', 'A', '1','2'. 
    "boo", "foo" .... these are illegal
    or precisely the node size is smaller then 2 ^ (its height) - 1 *)

(** give height info into the node, not including Empty 
for example the right node is at height 2 rather then 1
Height
   3               top
                /       \ 
   2        left        right
            /  \         /  \
   1      ll   Empty  Empty Empty
        /   \      
     Empty Empty *)
let rec tag_height_aux = function
  | Empty -> (0 , Empty)
  | Node (x, l, r) ->
    let (lh, l) = tag_height_aux l in 
    let (rh, r) = tag_height_aux r in
    let sub_height_max = max lh rh in
    let l = lift_height sub_height_max l in 
    let r = lift_height sub_height_max r in
    let h = 1 + sub_height_max in 
    (h, Node ((x, h), l, r))
and lift_height height = function
  | Empty -> Empty
  | Node ((x,h), l, r) as n -> 
    if h < height then 
        Node ((x, height), 
          lift_height (height - 1) l,
          lift_height (height - 1) r)
    else n 
let tag_height_colse_to_root t = snd (tag_height_aux t)
let height_to_arch h = 1 lsl (h - 1) - 1
let height_to_half_arch h = height_to_arch (h - 1)

(* [tree_span_easy2 depth base tree] 
   return (depth, left_span, xspan, right_span, constructed Tree) *)
let rec tree_span_easy2 depth base to_string = function
  | Empty -> (depth, 0, 0, 0, Empty)
  | Node ((x, xh), l, r) -> 
    let xs = to_string x in
    let xspan = String.length xs in
    let sub_tree_arch = (height_to_arch xh) in 
  
    let (ldepth, llspan, lvspan, lrspan, ln) = 
      tree_span_easy2 (depth + 1) base to_string l in  
    let left_span' = llspan + lvspan + lrspan in
    let left_span = max left_span' sub_tree_arch in 

    let rbase = base + left_span + xspan in   

    let (rdepth, rlspan, lvspan, lrspan, rn) = 
      tree_span_easy2 (depth + 1) rbase to_string r in

    let right_span' =  rlspan + lvspan + lrspan in
    let right_span = max right_span' sub_tree_arch in
    
    (depth, left_span, xspan, right_span, Node ((x,base + left_span, depth), ln, rn))

let rec horizontal_move delta = function
  | Empty -> Empty
  | Node ((x, base, depth), l, r) ->
    Node ((x, base + delta, depth), 
      horizontal_move delta l, 
      horizontal_move delta r) 

let rec leftest_base  = function
  | Empty -> 0
  | Node ((x, base, depth), Empty, _) -> base
  | Node ((x, base, depth), l, r) -> leftest_base l 

let layout_binary_tree_2 tree =  
  let (_,_,_,_, n ) = 
  tree |> tag_height_colse_to_root 
       |> tree_span_easy2 1 1 Char.escaped  
in 
  if leftest_base n > 1 
  then horizontal_move (1 - leftest_base n) n 
  else n 

(** <57> Layout a Binary Tree (3) *)
let example_layout_tree3 =
  let leaf x = Node (x, Empty, Empty) in
  Node ('n', Node ('k', Node ('c', leaf 'a',
                           Node ('h', Node ('g', leaf 'e', Empty), Empty)),
                 leaf 'm'),
       Node ('u', Node ('p', Empty, Node ('s', leaf 'q', Empty)), Empty))

let example_layout_tree4 =
    Node ('1', 
      Node ('2', Empty, Empty),
      Node ('3', 
        Node ('4',
          Node ('5', 
            Node ('6', Empty,Empty),
            Empty),
          Empty),
        Empty))

(* the old method could not handle the situation
   when the previous situation will be changed by current situation. *)

let rec translate_x delta = function
  | Empty -> Empty
  | Node ((v, x, y), l, r) ->
    Node ((v, x + delta, y), 
      translate_x delta l,
      translate_x delta r)    
(* hardest part to understand 
   what is the lr and rl. 
      lr is the left sub-tree's rightest x list
      rl is the right sub-tree's leftest x list
      the left and right sub-trees are 0 temporarily.
   why lrx - rlx ? 
      it means overlapping the two sub-trees at their root node,
      and minus every depth rightest and leftest x value
      on one depth, 
        if lrx - rlx = 0, it means there is no extra distance to move,
        if lrx - rlx > 0, it means right sub-tree's leftest node 
                                is nested into the left sub-tree.
        if lrx - rlx < 0 , it may result in shrink the both sub-tree but 
                            it is finding max dist and there is alaways 0 
                                form the root nodes and [].
                            This situation will be negleted.
  why max ? 
      find out the biggest horizontal distance of two sub-trees to tanslate 
      and  make them non-overlapping*)
let rec distance_to_move lr rl = match lr, rl with
  | lrx :: lrt , rlx :: rlt -> max (lrx - rlx)  (distance_to_move lrt rlt)
  | [], _ | _, [] -> 0 

(* merge p2 to p1, p1 and p2 is translated before nodes
  for ith elt, if p1 has i elt , choose the elt in p1, 
               if p1 do not has i elt, choose the i elt in p2
eg: merge ll rl , left sub-tree is 2, right sub-tree is 3
    only if left sub-tree profile : [-1; -2] has ended, 
      the right sub-tree profile : 1::0::[1] could be counted. 
    ->  1         1      1 <-
    ->           / \       <-
    ->  2      2   3     3 <-
    ->        /   / \      <-
    ->  4    4   5   6   6 <-
    ->              /      <-
    ->  9          9     9 <- *)
let rec merge_profile p1 p2 = match p1, p2 with
  | x1 :: tl1 , _x2 :: tl2 -> x1 :: merge_profile tl1 tl2
  | [], _ -> p2 
  | _, [] -> p1  

(*
layout return:
left profile    tree    right_profile 
      x of               x of 
    ->  1         1      1 <-
    ->           / \       <-
    ->  2      2   3     3 <-
    ->        /   / \      <-
    ->  4    4   5   6   6 <-
    ->              /      <-
    ->  9          9     9 <-   *)
let rec layout depth = function
  | Empty -> ([], Empty, [])
  | Node (v, l, r) ->
    let (ll, l', lr) = layout (depth + 1) l in 
    let (rl, r', rr) = layout (depth + 1) r in
    (* /2 : floor, but alway move left and right 1 index *)
    let d = 1 + distance_to_move lr rl / 2 in
    let ll = List.map (fun x -> x - d) ll in
    let lr = List.map (fun x -> x - d) lr in
    let rl = List.map ((+) d) rl in 
    let rr = List.map ((+) d) rr in
    (* only merge ll rl and merge rr lr 
       because ll lr and rr rl are merged in the recursive progress,
       and make ll lr, rl rr are of equal length to each ohter*)
    (0 :: merge_profile ll rl,
     Node ((v, 0, depth), translate_x (-d) l', translate_x d r'),
     0 :: merge_profile rr lr)
let layout_binary_tree_3 tree = 
  let (l, tree', r) = layout 1 tree in
  let x_leftest = List.fold_left min 0 l in 
  translate_x (1 -x_leftest) tree'