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
    List.iteri (fun i e -> if i <= n - k then Stack.push (1, e, [e]) stack else ()) lst;
    (* simulate the combination *)
  while not ( Stack.is_empty stack) do
    let len, last, cur = (Stack.pop stack) in 
    if len = k then res := cur :: !res
    else 
      let index_last =  Hashtbl.find hash last in 
        for i = index_last + 1 to len + n - k do
          (* if len + n - i >= k then *)
          Stack.push ( len + 1, arr.(i), arr.(i) :: cur) stack 
        done  
  done;
  !res

(** <27> Group the Elements of a Set Into Disjoint Subsets  *)

(* index form 0 *)
let rec decri lst i = 
  match lst with
  | [] -> []
  | h :: t -> 
    if i = 0 && h > 0 then (h - 1) :: t 
    else h :: (decri t (i - 1))

(* [appendi lst i e] is [lst] which is two-level nested list, eg. [[1]; [2;3]]
  with the element list whose index is [i] prepend [e]. 
  the index of [lst] is form 0
  Example: 
    let lst =  [[1]; [2;3] [4;5;6]] 
    appendi lst 1 4
    get [[1]; [4;2;3]; [4;5;6]] *)
let rec appendi lst i e =
  match lst with
  | [] -> []
  | h :: t ->
    if i = 0 then (e :: h) :: t
    else h :: (appendi t (i - 1) e)

let rec div_lst_in_sets_by_sizes acc cur lst sizes = 
  if List.for_all (fun x -> x = 0) sizes then  cur :: acc
  else
    match lst with
    | [] -> acc
    | h :: t ->
      let take_h = distribute_to_each_set acc cur h t sizes in 
      let skip_h = div_lst_in_sets_by_sizes acc cur t sizes in
      take_h @ skip_h

and distribute_to_each_set acc cur h t sizes =
  let take_acc = ref [] in
  List.iteri (
    fun i e -> 
      if e <> 0 then 
        let new_size = decri sizes i in  
        let new_cur = appendi cur i h in
        let res = div_lst_in_sets_by_sizes acc new_cur t new_size in
        take_acc := res @ !take_acc
      )
  sizes ;
  !take_acc

let group lst sizes = 
  let init_cur = List.map (fun _ -> []) sizes in
  div_lst_in_sets_by_sizes [] init_cur lst sizes


(* <27> interation solution *)
let sum_of_list = 
    List.fold_left (fun acc e -> acc + e) 0
let sum_of_llst =
    List.fold_left (fun acc l -> acc + sum_of_list l) 0 

(** Require: elts in [lst] is unique.
            elts in sizes is not zero. *)
let div_list_into_sets_iter lst sizes = 
  let res = ref [] in
  (* the amount of elts in all subsets *)
  let k = sum_of_list sizes in 
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
    (* for each elt in lst *)
    List.iteri (fun ilst e -> if ilst <= n - k then 
      (* put elt into each subset *)
      List.iteri (fun i size -> 
        (* make stack elt: *)
        if size <> 0 then 
          let cur = List.map (fun _ -> []) sizes in
          let cur = appendi cur i e in 
          let sizes = decri sizes i in 
          (* the stack elt is len last sizes cur *)
          Stack.push (1, e, sizes, cur) stack 
        ) sizes
      ) lst;
    (* simulate the combination *)
  while not ( Stack.is_empty stack) do
    let len, last, sizes, cur = Stack.pop stack in 
    if len = k then res := cur :: !res
    else 
      let index_last =  Hashtbl.find hash last in 
        for ilst = index_last + 1 to len + n - k do
          (* if len + n - i >= k then *)
            List.iteri (fun i size -> 
              (* make stack elt: *)
              if size <> 0 then 
                let cur = appendi cur i arr.(ilst) in 
                let sizes = decri sizes i in 
                (* the stack elt is len last sizes cur *)
                Stack.push (len + 1, arr.(ilst), sizes, cur) stack 
              ) sizes;
        done  
  done;
  !res

let group_iter lst sizes =
  div_list_into_sets_iter lst sizes



(* <28> Sorting a List of Lists According to Length of Sublists  *)
(* using the built-in sort function which is merge sort 
   but which consists of sort, rev_sort, rev_merge, rev_merge_rev
   because of list insert and remove from one side like stack. *)
let length_sort lst = 
  let lenlst = List.map (fun e -> (List.length e, e)) lst in 
  let lenlst_sorted = List.sort compare lenlst in
  List.map (fun (_, e) -> e) lenlst_sorted


let frequency_sort lst = 
  let lenlst = List.map (fun e -> (List.length e, e)) lst in
  (* count the frequency of length *)
  let open Hashtbl in 
  let h = create 16 in
  List.iter (fun (l, _) -> 
    if mem h l then add h l ((find h l) + 1) else add h l 1 )  lenlst;
  (* give the frequency tag to each elt list *)
  let freqlst = List.map (fun (l, e) -> (find h l ,l,e)) lenlst in
  (* sort and remove freqency and len tag *)
  let freqlst_sorted = List.sort 
      (fun (fre1, _, e1) (fre2, _, e2) -> 
        if fre1 = fre2 then compare e1 e2 else fre1 - fre2) freqlst in
  List.map (fun (_,_, e) -> e) freqlst_sorted

