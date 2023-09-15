open G_44to49

(** <58> A String Representation of Binary Trees *)
let example_layout_tree5 =
  let leaf x = Node (x, Empty, Empty) in
    (Node ('a', Node ('b', leaf 'd', leaf 'e'),
     Node ('c', Empty, Node ('f', leaf 'g', Empty))))

let tree5_string = "a(b(d,e),c(,f(g,)))"      

let rec string_of_tree_aux to_string = function 
  | Empty -> ""
  | Node (x, l, r) -> 
    let xs = to_string x in
    let ls = string_of_tree_aux to_string l in
    let rs = string_of_tree_aux to_string r in
    if l = Empty && r = Empty then 
     xs 
    else 
    xs ^ "(" ^ ls ^ "," ^ rs ^")"     

let string_of_tree = string_of_tree_aux Char.escaped    


