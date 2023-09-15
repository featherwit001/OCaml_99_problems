open G_44to49
let id x = x 
let max_col = 110

(* Base of left subtree and root is 0; 
   Base right subtree is left substree span + 1 
   0 ----------- root------------- Base
   1         ^           ^
   2       /  \        /  \
   3     /     \     /     \
   4   /________\  /________\
   5 
   6
depth
*)
(* calculate the [t] left-right span 
   Require: *)

(** [tree_span2 tree depth base stack to_string] is 
    (depth, left_subtree_span, root_span, right_subtree_span) *)
let rec tree_span2 t depth base stack to_string = 
  match t with 
  | Empty -> (depth, 0, 0, 0)
  | Node(x, l, r) -> 
    let x = to_string x in 
    let xspan = String.length x in 

    (* get left subtree  *)
    let (ldepth, llspan, lvspan, lrspan) = tree_span2 l (depth + 2) base stack to_string in
    let leftspan = llspan + lrspan + lvspan in 
    let root_right_base = base + leftspan + xspan in
    let root_left_base = base + leftspan in  
    
    let (rdepth, rlspan, rvspan, rrspan) = tree_span2 r (depth + 2) root_right_base stack to_string in
    (* Printf.printf "root = %s depth %d " x depth;
    Printf.printf "left depth %d "  ldepth;
    Printf.printf "right depth %d\n" rdepth; *)
    
    let rightspan = rlspan + rrspan + rvspan in
    
    let left_child_left_base = base + llspan in 
    let left_child_right_base = base + llspan + lvspan in 
    let right_child_left_base = base + leftspan + xspan + rlspan in 
    let slashbase = if lvspan = 1 && xspan = 1 then left_child_left_base 
                    else (left_child_right_base + root_left_base + 1) / 2 in 
    let backslashbase = (right_child_left_base + root_right_base) / 2 in 
    
    if l <> Empty then begin 
      (* Printf.printf "root = %s root_left_base = %d leftbase = %d slashbase = %d\n" 
        x root_left_base left_child_left_base slashbase; *)
      Stack.push (depth + 1, slashbase, "/", 1) stack
    end;
    if r <> Empty then begin 
      (* Printf.printf "root = %s root_rightbase = %d rightbase = %d backslashbase = %d\n" 
        x root_right_base right_child_left_base backslashbase; *)
      Stack.push (depth + 1, backslashbase, "\\", 1) stack;  
    end;

    let x_offset = if  l <> Empty && r <> Empty 
                   then (slashbase + backslashbase ) / 2 - xspan / 2
                   else base + leftspan 
    in 
    Stack.push (depth,  x_offset, x, xspan) stack;
    (max ldepth rdepth, leftspan, xspan, rightspan)


let fill_blank n =
  if n <= 0 then ""
  else 
  String.make n ' ' 

let rec fill_newline acc n =
  if n = 0 then acc
  else fill_newline (acc ^ "\n") (n - 1) 

 (* 
   render cordinate system
   init col line with -1 -1 means nothing in the cordinate system
     0 1 2 3 4 5 6 7 8 9 col
   0
   1  
   2  
   3  
   4  
   5 
   6 
  line*)
let render lst =
  let rec render_aux acc cur line col = function
    | [] -> acc ^ cur
    | (d, b, s, slen) :: t -> 
      (* Printf.printf "line = %d, d = %d s = %s \n"line  d s; *)
      if d > line then
        render_aux (fill_newline (acc ^ cur) (d - line)) ((fill_blank (b)) ^ s) d (b + slen -1) t
      else if d = line then
        render_aux acc (cur ^ (fill_blank (b - col - 1))^ s) line (b + slen -1) t
      else failwith "the coordinate system point is not ordered!" 
      in
    render_aux "" "" (-1) (-1) lst
  
let binary_tree_to_cordinate_system elt_to_string t =
  let stack = Stack.create () in
  let (d,lspan, xspan, rspan) = tree_span2 t 0 0 stack elt_to_string in 
  let range = (0, d, 0, lspan + xspan + rspan) in
  let points = stack 
    |> Stack.to_seq |> List.of_seq |> List.sort_uniq compare 
  in (range, points) 

let print_binary_tree elt_to_string t = 
  t |> binary_tree_to_cordinate_system elt_to_string 
    |> snd 
    |> render |> print_endline

(* -- print binary trees -- *)

let horizontal_translation delta (range, lst)  = 
  let (up, down, left, right) = range in 
  if left + delta < 0 && right + delta > max_col then failwith "out of max_col limit"
  else 
    let lst_moved = 
          lst |> List.map (fun (depth, base, s, slen) -> (depth, base + delta,s ,slen))
  in ((up,down,left + delta, right + delta), lst_moved)  

let vertical_translation  delta (range, lst)= 
  let (up, down, left, right) = range in 
  if up + delta < 0 then failwith "out of min_line limit "
  else 
    let lst_moved = 
      lst |> List.map (fun (depth, base, s, slen) -> (depth + delta, base,s ,slen))
  in ((up + delta, down + delta,left, right), lst_moved)

let rec arrange_points_aux ?(internal=3) acc max_vertical_range_in_this_set depth base = function
  | [] -> ((0, depth, 0, base ) , List.stable_sort compare acc) 
  | ((range, points) as shapes) ::t ->
    let (up, down, left, right) = range in
    if base + (right - left + 1) > max_col 
    then
      let base' = 0 in
      let depth' = depth + max_vertical_range_in_this_set in
      let max_vertical_range_in_this_set' = down - up + 1 in
      let (new_range, new_points) = 
        shapes 
        |> horizontal_translation (base' - left) 
        |> vertical_translation (depth' - up)
      in 
      let base'' = base' + right - left + 1 + internal in 
      let acc' = new_points @ acc in
      arrange_points_aux acc' max_vertical_range_in_this_set' depth' base'' t  
    else
      let (new_range, new_points) = 
        shapes 
        |> horizontal_translation (base - left) 
        |> vertical_translation (depth - up)
      in
      let base' = base + right - left + 1 + internal in
      let max_vertical_range_in_this_set' =  
            max max_vertical_range_in_this_set (down - up + 1) in 
      let acc' = new_points @ acc in 
      arrange_points_aux acc' max_vertical_range_in_this_set' depth base' t

(* input (range * points) list
   range = (up,, down, left, right)
   point = (depth, base , s , slen) depth = y, base = x  *)
let arrange_points p = arrange_points_aux [] 0 0 0 p

let trees_to_points elt_to_string trees =
  trees 
  |> List.map (
      fun t -> 
        let stack = Stack.create () in 
        let (d,lspan, xspan, rspan) = tree_span2 t 0 0 stack elt_to_string in 
        let range = (0, d, 0, lspan + xspan + rspan) in
        let points = 
          stack |> Stack.to_seq |> List.of_seq |> List.sort_uniq compare in
        (range, points)
        )

let print_binary_trees elt_to_string trees = 
  trees
  |> trees_to_points elt_to_string
  |> arrange_points
  |> snd
  |> render
  |> print_endline
  

let print_char_binary_trees = print_binary_trees Char.escaped  
  

(* tag each node with x y . x form x_init , y form y_init*)
let layout_binary_tree_compressed ?(x_init=0) ?(y_init=0) ?(d_step=2) to_string = 
  let rec translate_x delta = function
    | Empty -> Empty
    | Node ((v, vlen, x, y), l, r) ->
      Node ((v, vlen, x + delta, y), translate_x delta l, translate_x delta r) in
  let rec distance_to_move lr rl = match lr, rl with
    | lrx :: lrt , rlx :: rlt -> max (lrx - rlx) (distance_to_move lrt rlt) 
    | [], _ | _, [] -> 0 in
  let rec merge_profile p1 p2 = match p1, p2 with
    | x1 :: tl1, x2 :: tl2 -> x1 :: merge_profile tl1 tl2
    | [], _ -> p2
    | _, [] -> p1 in
  let rec layout depth = function
    | Empty -> ([], Empty, [])
    | Node (v, l, r) -> 
      let v = to_string v in 
      let vlen = String.length v in 
      let ll, l', lr = layout (depth + d_step) l in 
      let rl, r', rr = layout (depth + d_step) r in
      let d = 1 + distance_to_move lr rl / 2 in  (* key *)
      let ll = List.map (fun x -> x - d) ll in
      let lr = List.map (fun x -> x - d) lr in
      let rl = List.map ((+) d) rl in 
      let rr = List.map ((+) d) rr in 
      let lprofile = merge_profile ll rl in
      let rprofile = merge_profile rr lr in 
      (-(vlen / 2) :: lprofile,
       Node ((v, vlen, -(vlen / 2), depth), translate_x (-d) l', translate_x d r'),
       ((vlen + 2 - 1) / 2) :: rprofile) 
    in 
    fun tree -> let l, tree', r = layout y_init tree in
                let x_min = List.fold_left min 0 l in
                let x_max = List.fold_left max 0 r in
                let y_max = (max (List.length l) (List.length r)) * d_step in 
                let tree'' = translate_x (x_init - x_min) tree' in
                let range = (y_init, y_max, x_init, (abs x_min) + abs (x_max)) in 
                (range, tree'')

let rec layout_tree_to_points_aux px py stack  = function
  | Empty -> (px, py)
  | Node ((s,slen, x, y), l, r) ->
    Stack.push (y, x, s, slen) stack;
    let (lx, ly) = layout_tree_to_points_aux x y stack l in 
    let (rx, ry) = layout_tree_to_points_aux x y stack r in
    (* floor *)
    if l <> Empty then begin Stack.push (y + 1, (lx + x + slen/2 ) / 2, "/", 1) stack end;
    (* ceiling *)
    if r <> Empty then begin Stack.push (y + 1, (rx + x + slen/2 + 2 - 1) / 2, "\\", 1) stack end;
    (x + slen/2, y)

let layout_tree_to_points tree = 
  let stack = Stack.create () in
  ignore (layout_tree_to_points_aux 0 0 stack tree); 
  stack |> Stack.to_seq |> List.of_seq |> List.sort compare 


let print_binary_tree_compressed to_string tree = 
  let range, layout_tree = layout_binary_tree_compressed to_string tree in
  let points = layout_tree_to_points layout_tree in
  render points |> print_endline
  

let print_binary_trees_compressed to_string trees =
  trees 
  |> List.map (
      fun tree -> 
        let range, layout_tree = layout_binary_tree_compressed to_string tree in 
        let points = layout_tree_to_points layout_tree in
        (range, points)
        )
  |> arrange_points
  |> snd
  |> render
  |> print_endline
  
let tree0 = Node ("1", Empty, Empty)
let trees1 = [tree0; tree0]



let treel = Node ("1", 
              Node("2", Empty, Empty), 
              Empty)
let treer = Node ("1", 
              Empty,
              Node("2", Empty, Empty))
let treerr = Node ("1", 
                Empty,
                Node("2", 
                  Empty, 
                  Node("3", Empty, Empty)))
                                    
let trees2 = [treer; treer ]

let tree1 = Node ("1", 
              Node ("2", Empty, Empty),
              Node ("3", Empty, Empty))
let tree2 = Node ("1", 
              Node ("2", Empty, Empty),
              Node ("3", 
                Node ("4", Empty, Empty), 
                Empty))
let tree3 = Node ("1", 
                Node ("2", 
                  Empty, 
                  Node("3", Empty, Empty)),
                Empty)
let tree3' = Node ("1", 
                Node ("2",
                  Node ("4", Empty, Empty), 
                  Node("3", Empty, Empty)),
                Empty)               
let tree3'' = Node ("1", 
                Node ("2", 
                  Node ("4", Empty, Empty), 
                  Empty),
                Node("3", Empty, Empty))                  
let tree4 = Node ("1", 
                Node ("2", Empty, Empty),
                Node ("3", 
                  Node ("4", 
                    Empty, 
                    Node("5", Empty, Empty)), 
                  Empty)) 
let tree5 = Node ("1", 
              Node ("2", 
                Node ("4", Empty, Empty), 
                Node("5", 
                  Node("8",Empty,Empty), 
                  Empty)), 
              Node ("3", 
                Node("6",
                  Node ("9",Empty,Empty),
                  Empty),
                Node("7", Empty, Empty)))
          
let huffman_tree = 
  Node ("100",
    Node ("a", Empty, Empty),
    Node ("55",
      Node ("25",
        Node ("c", Empty, Empty),
        Node ("b", Empty, Empty)),
      Node ("30",
        Node ("14",
          Node ("f", Empty, Empty),
          Node ("e", Empty, Empty)),
        Node ("d", Empty, Empty))))               

let htree_easy =
      Node ("55",
        Node ("25",
          Node ("c", Empty, Empty),
          Node ("b", Empty, Empty)),
        Node ("30",
          Node ("14",
            Node ("f", Empty, Empty),
            Node ("e", Empty, Empty)),
          Node ("d", Empty, Empty)))


let trees3 = [treer; huffman_tree;  tree5; tree4]