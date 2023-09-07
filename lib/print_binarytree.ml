open G_44to50 

let id x = x 
let max_col = 100

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
let rec tree_span t depth base stack to_string = 
  match t with 
  | Empty -> (depth, 0, 0, 0)
  | Node(x, l, r) -> 
    let x = to_string x in 
    let xspan = String.length x in 

    (* get left subtree  *)
    let (ldepth, llspan, lvspan, lrspan) = tree_span l (depth + 2) base stack to_string in
    let leftspan = llspan + lrspan + lvspan in 
    let root_right_base = base + leftspan + xspan in
    let root_left_base = base + leftspan in  

    let (rdepth, rlspan, rvspan, rrspan) = tree_span r (depth + 2) root_right_base stack to_string in
    let rightspan = rlspan + rrspan + rvspan in

    let left_child_left_base = base + llspan in 
    let right_child_left_base = base + leftspan + xspan + rlspan in 
    let slashbase =  (left_child_left_base + lvspan + root_left_base ) / 2 in 
    let backslashbase = (right_child_left_base + root_right_base) / 2 in 
    Stack.push (depth, base + leftspan, x, xspan) stack;
    
    if l <> Empty then begin 
      Printf.printf "root = %s leftspan = %d root_left_base = %d leftbase = %d slashbase = %d\n" 
        x leftspan root_left_base left_child_left_base slashbase;
      Stack.push (depth + 1, slashbase, "/", 1) stack
    end;
    if r <> Empty then begin 
      Printf.printf "root = %s root_rightbase = %d rightbase = %d backslashbase = %d\n" 
        x root_right_base right_child_left_base backslashbase;
      Stack.push (depth + 1, backslashbase, "\\", 1) stack;  
    end;
    (depth, leftspan, xspan, rightspan)


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
let render (range, lst) =
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
    |> render |> print_endline

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

let rec arrange_points_aux acc max_vertical_range_in_this_set depth base = function
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
      let base'' = base' + right - left + 1 + 5 in 
      let acc' = new_points @ acc in
      arrange_points_aux acc' max_vertical_range_in_this_set' depth' base'' t  
    else
      let (new_range, new_points) = 
        shapes 
        |> horizontal_translation (base - left) 
        |> vertical_translation (depth - up)
      in
      let base' = base + right - left + 1 + 5 in
      let max_vertical_range_in_this_set' =  
            max max_vertical_range_in_this_set (down - up + 1) in 
      let acc' = new_points @ acc in 
      arrange_points_aux acc' max_vertical_range_in_this_set' depth base' t

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