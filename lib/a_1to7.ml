(** <1> Tail of a List *)
let rec last = function
  | [] -> None
  | [h] -> Some h
  | _h :: t -> last t

(** <2>  *)
let rec last_two = function
  | [] | [_] -> None
  | h1 :: h2 :: [] -> Some (h1, h2)
  | _h :: t  -> last_two t

(** <3>  *)
  let rec nth lst n =
  match lst with
  | [] -> failwith "nth"
  | h :: t -> if n = 0 then h else nth t (n - 1) 

let rec length_aux acc = function
  | [] -> acc
  | _h :: t -> length_aux (acc + 1) t

(** <4>  *)
let length lst = length_aux 0 lst

(** append elts in lst1 to lst2  *)
let rec append_rev lst1 lst2 = 
  match lst1 with
  | [] -> lst2
  | h :: t ->  append_rev t (h :: lst2)

(** <5>  *)
let rev lst = append_rev lst []

(** <6>  *)
let is_palindrome lst =
  lst = rev lst

type 'a node =
  | One of 'a 
  | Many of 'a node list


(* let rec flatten = function
  | [] -> []
  | (One x) :: t -> x :: (flatten t)
  | (Many l) :: t -> flatten l @ flatten t *)

(* <7> *)
let rec flatten_aux acc = function
  | [] -> acc
  | (One x) :: t ->  flatten_aux (x :: acc) t
  | (Many l) :: t ->   flatten_aux (flatten_aux acc l) t

let flatten lst = List.rev (flatten_aux [] lst)

