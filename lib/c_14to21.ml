(** <14> duplicate  *)
let rec duplicate_aux acc = function
  | [] -> acc
  | h :: t -> duplicate_aux (h :: h :: acc) t

let duplicate lst = List.rev (duplicate_aux [] lst)


(** <15> Replicate the Elements of a List a Given Number of Times  *)
let rec repeat acc c n =
  if n = 0 then acc else repeat (c :: acc) c (n - 1)

let rec replicate_aux acc lst n =
  match lst with
  | [] -> acc
  | h :: t -> replicate_aux (repeat acc h n) t n

let replicate lst n = List.rev (replicate_aux [] lst n)


(** <16'> drop nth element  *)

(*Require : n >= 1  *)
(* let rec drop_aux acc lst n =
  if n = 1 then
    match lst with
    | [] -> List.rev acc
    | _h :: t -> List.rev acc @ t
  else if n > 1 then
    match lst with
    | [] -> List.rev acc
    | h :: t -> drop_aux (h :: acc) t (n - 1)
  else failwith "wrong nth" *)

  (* just simplify the drop_aux above *)
  (* the improve version is <20> *)
let rec drop_aux' acc lst n = 
  match lst, n with
  | [], _n -> List.rev acc
  | _h :: t , 1 -> List.rev acc @ t
  | h :: t , n when n > 1 -> drop_aux' (h :: acc) t (n - 1)
  | _lst , _n -> failwith "wrong nth" (* _n is lower than 1*) 


let drop_nth lst nth = drop_aux' [] lst nth


(** <16>  Drop Every N'th Element From a List *)

(* nth from 1 *)
let rec drope_aux acc count lst nth = 
  match lst with
  | [] -> List.rev acc
  | h :: t -> if count = nth 
              then drope_aux acc 1 t nth
              else drope_aux (h :: acc) (count + 1) t nth

let drope lst nth = drope_aux [] 1 lst nth
 

(** <17>  Split a List Into Two Parts; The Length of the First Part Is Given *)
(* [n] is the amount of elts *)
let rec splite_aux acc lst n = 
  match lst, n with
  | [], _n -> (List.rev acc, [])
  | lst, 0 -> (List.rev acc, lst)
  | h :: t, n -> splite_aux (h :: acc) t (n - 1) 

let split lst n = splite_aux [] lst n

(** <18> Extract a Slice From a List *)
(* index form 0, s: start e: end are both indexs.
   require:  0 <= s <= e *)
let rec slice_aux acc lst s e = 
  match lst with
    | [] -> List.rev acc  (* end of rec *)
    | h :: t -> if s = 0 && e = 0 then List.rev (h :: acc) (* take the last one and end*)
           else if s > 0 && e > 0 then slice_aux acc t (s - 1) (e - 1) (* need to drop*)
           else if s = 0 && e > 0 then slice_aux (h :: acc) t 0 (e - 1) (* need to take*)
           else failwith "requirement violence"

let slice lst s e = slice_aux [] lst s e

(* index form 0, this is a version which is more easy to understand
   require:  0 <= s <= e *)
let slice_easy lst s e = 
  (* [drop n lst] is the [lst] without the first [n] elts
     [n] means the amount of elts to be drop *)
  let rec drop n = function 
    | [] -> []
    | _h :: t as lst -> if n = 0 then lst else drop (n - 1) t
  in
  (* [take acc n lst] is the first [n] elts of [lst] *)
  let rec take acc n = function
    | [] -> List.rev acc
    | h :: t -> if n = 0 then List.rev acc else take (h :: acc) (n - 1) t
  in
  take [] (e - s + 1)  (drop s lst)

(* drop and take is similar, so they can be abstract 
   but which is excessive abstraction*)


(** <19>  Rotate a List N Places to the Left *)
(* [n] is the amount of the elements *)
let rotate lst n = 
  let first_half, second_half = split lst n in
  second_half @ first_half

(** <20> Remove the K'th Element From a List 
    look at <16'>*)

(* hhhh easy implement, 
   but take care of the difference between index and number  *)
let remove_at' i lst = drop_nth lst (i + 1)  

(* without tail_recursive but faster *)
let rec remove_at'' n = function
    | [] -> []
    | h :: t -> if n = 0 then t else h :: remove_at'' (n - 1) t;;


(* index form 0 *)
let rec remove_at_aux acc i = function
  | [] -> acc
  | h :: t -> if i = 0 then (List.rev acc) @ t 
                       else remove_at_aux (h :: acc) (i - 1) t
let remove_at i lst = remove_at_aux [] i lst 


(** <21> Insert an Element at a Given Position Into a List *)
let rec insert_at e i = function
  | [] -> []
  | h :: t as l-> if i = 0 then e :: l else h :: (insert_at e (i - 1) t)



