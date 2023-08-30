(** <22> Create a List Containing All Integers Within a Given Range  *)
(* i <= j *)
let rec from acc i j = if i <= j then from (j :: acc) i (j - 1) else acc

let range i j = from [] i j


(* <23> Extract a Given Number of Randomly Selected Elements From a List  *)

(* index from 0 *)
let rec extract acc i = function
  | [] -> raise Not_found
  | h :: t -> if i = 0 then h, (List.rev acc) @ t else extract (h :: acc) (i - 1) t

let rec rand_select_aux acc lst n len =
  if n = 0 then acc
  else let e, rest = extract [] (Random.int len) lst in
    rand_select_aux (e :: acc) rest (n - 1) (len - 1)
  
let rand_select lst n = rand_select_aux [] lst n (List.length lst)


(** <24>  Draw N Different Random Numbers From the Set 1..M*)
let lotto_select n m = rand_select (range 1 m) n

(** <25> Generate a Random Permutation of the Elements of a List  *)

let rec permutation_aux acc lst len =
  if len = 0 then acc else 
    let e , rest = extract [] (Random.int len) lst in
     permutation_aux (e :: acc) rest (len - 1)

let permutation lst = permutation_aux [] lst (List.length lst)


(** <26>  Generate the Combinations of K Distinct Objects 
     Chosen From the N Elements of a List *)

(* 
(* the max is C (10, 20) *)
let rec extract_combination k lst = 
  if k <= 0 then [[]]
  else match lst with
    | [] -> []
    | h :: t -> 
      let with_h = List.map (fun elt -> h :: elt) (extract_combination (k - 1) t) in
      let without_h = extract_combination k t in 
    with_h @ without_h *)

let rec combination_aux acc cur k lst = 
  if k <= 0 then cur :: acc
  else match lst with
    | [] -> acc (* cur is shorter than k *)
    | h :: t -> 
      let with_h = combination_aux acc (h :: cur) (k - 1) t in
      let without_h = combination_aux acc cur k t in 
    with_h @ without_h

let extract_combination k lst = combination_aux [] [] k lst


(* index form 0 , elts must be uniq*)
(* the easy version is in the ../combinations.py 
  which includes the iter funciton and simulation*)
let combination_iter k lst = 
  let res = ref [] in
  let lst = List.sort_uniq compare lst in
  (* get elt form index *)
  let arr = Array.of_list lst in
  (* get index form elt *)
  let hash = lst 
             |> List.mapi (fun i e -> (e, i)) 
             |> List.to_seq
             |> Hashtbl.of_seq
  in
  let n = Array.length arr in 
  let stack = Stack.create () in 
    (* n - k means containing the last elt 
    which can be conbined with the rest of elts to meet the k elts *)
    List.iteri (fun i e -> if i <= n - k then Stack.push (1, [e]) stack else ()) lst;
    (* simulate the combination *)
  while not ( Stack.is_empty stack) do
    let len , cur = (Stack.pop stack) in 
    if len = k then res := cur :: !res
    else 
      match cur with
      | [] -> failwith "cur is emtpy impossible"
      | h :: _t as cur-> let indexh =  Hashtbl.find hash h in 
        for i = indexh + 1 to len + n - k do
          (* if len + n - i >= k then *)
          Stack.push ( len + 1, arr.(i) :: cur) stack 
        done  
  done;
  !res
