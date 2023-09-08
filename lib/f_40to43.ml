(** <40> Truth Tables for Logical Expressions (2 Variables)   *)
type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr;;

module ENV = Map.Make(String)  

let bind env id value =
  ENV.add id value env

let rec eval env expr =
  match expr with
  | Var x ->  lookup env x 
  | Not e -> not (eval env e)
  | And (a, b) -> (eval env a )&&(eval env b)
  | Or (a, b) -> (eval env a ) || (eval env b)

and lookup env x =
  if ENV.mem x env then ENV.find x env 
  else failwith (x ^ " is not bound")

let table2 a b expr = 
  let possible_scenarios = 
    [(true,true); (true,false); (false,true); (false,false)] in
  possible_scenarios 
    |> List.map (
        fun (x, y) -> 
          let env = ENV.empty in
          let env = bind env a x in
          let env = bind env b y in 
          (x, y, eval env expr)) 

(** <41>  Truth Tables for Logical Expressions  *)
let rec make_possible_scenarios acc cur = function
    | [] -> (List.rev cur) :: acc (* means the variables number is less than n. impossible*)
    | h :: t -> 
      let take_t = make_possible_scenarios acc ((h,true)::cur) t  in
      let take_f = make_possible_scenarios acc ((h,false)::cur) t in
      take_t @ take_f

let rec binds env = function
  | [] -> env
  | (id, value) :: t -> 
    binds (bind env id value) t 

let table varlst expr =
  (make_possible_scenarios [] [] varlst)
  |> List.map (
      fun idvalues -> 
        let env = binds ENV.empty idvalues in
        (idvalues, eval env expr))

(** <42> Gray Code *)
let gray n =
  let rec gray_aux acc n =
    if n = 1 then acc
    else 
      let fst_half = acc |> List.map (fun e -> "0" ^ e) in
      let snd_half = acc |> List.rev |> List.map (fun e -> "1" ^ e) in
      gray_aux (fst_half @ snd_half ) (n - 1)
    in 
    gray_aux ["0"; "1"] n 


(* <42> iteration solution *)
let change n = if n = "0" then "1" else "0"
let rec change_right_first_one's_left = function
  | [] -> [] (*impossible*)
  | [h] -> [h] (*impossible*)
  | h1 :: (h2 :: t' as t) -> 
    if h1 = "1" then h1 :: (change h2) :: t'
    else
      h1 :: change_right_first_one's_left t
let change_rightest = function
  | [] -> [] (*impossible*)
  | h :: t -> (change h) :: t

let save cur = List.fold_right (fun e acc -> acc ^ e ) cur ""
(* cur is store gray code form right to left *)
let rec gray_iter_aux acc cur n limit =
  if n = limit then acc
  else 
    let cur = 
      if n mod 2 = 1 then  
        change_rightest cur 
      else 
        change_right_first_one's_left cur
    in 
    gray_iter_aux ((save cur) :: acc) cur (n + 1) limit

let gray_iter n =
  let rec gray_init acc n = 
    if n = 0 then acc
    else gray_init ("0" :: acc) (n - 1)
  in 
  let cur = gray_init [] n in 
  let limit = int_of_float (2. ** (float_of_int n )) in
  List.rev (gray_iter_aux [save cur] cur 1 limit)

(** <43>  Huffman Code *)
open Heap  
open G_44to49
(* open Print_binarytree *)
(** Require: 
    symbols must be unique.
    huffman code must have at least two symbols *)
    
module HuffmanCode = struct 
  type hufftree = 
    | Leaf of string * int
    | Node of int * hufftree *  hufftree

  let rec to_binarytree = function
    | Leaf (s, i) -> G_44to49.Node(s , Empty, Empty)
    | Node (i, l, r) -> G_44to49.Node (string_of_int i,to_binarytree l, to_binarytree r)
  
  let to_string = function
    | Leaf (s, _) -> s
    | Node (w, _, _) -> string_of_int w  

  let createleaf s w = 
    Leaf (s, w)

  let merge_subtree l r =
    match l, r with
    | Leaf (s1,w1), Leaf (s2, w2) -> Node (w1 + w2,l, r)
    | Node (w1,_, _), Node(w2, _, _) -> Node (w1 + w2, l, r)
    | Leaf (s1,w1),  Node(w2, _, _) -> Node (w1 + w2, l, r)
    | Node (w1,_, _), Leaf (s2, w2) -> Node (w1 + w2, l, r)

  module HuffmanTree_Ord = struct
    type t = hufftree
    let compare t1 t2 = 
      match t1, t2 with
      | Leaf (s1,w1), Leaf (s2, w2) -> w1 -w2 
      | Node (w1,_, _), Node(w2, _, _) -> w1 - w2
      | Leaf (s1,w1),  Node(w2, _, _) -> w1 - w2
      | Node (w1,_, _), Leaf (s2, w2) -> w1 - w2
  end

  module MinHuffHeap = Make(HuffmanTree_Ord)

  let build_hufftree lst =
    let mh = lst 
            |> List.map (fun (s, w) -> createleaf s w) 
            |> MinHuffHeap.of_list 
    in 
    let out_loop = ref false in
    while not !out_loop do
      match MinHuffHeap.pop mh, MinHuffHeap.pop mh  with
      | None , _ -> failwith "impossible"
      | Some x , None -> MinHuffHeap.push x mh; out_loop := true
      | Some t1, Some t2 -> MinHuffHeap.push (merge_subtree t1 t2) mh
    done;
    let top = MinHuffHeap.pop mh in
      match top with 
      | None -> failwith "impossible"
      | Some t -> t
  
  let rec tree_to_code_aux acc cur t =
    match t with
    | Leaf (s, _) -> (s, cur) :: acc
    | Node (_, l, r) -> 
      let left_code = tree_to_code_aux acc (cur ^ "0") l in
      let right_code = tree_to_code_aux acc (cur ^ "1") r in
      left_code @ right_code
    
  let hufftree_to_huffcode t = 
    tree_to_code_aux [] "" t 
  let of_list = build_hufftree
  let to_list = hufftree_to_huffcode

end