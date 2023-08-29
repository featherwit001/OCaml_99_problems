(** <8>  Eliminate Duplicates  *)
let rec compress_aux acc = function
  | [] -> [] (* only [] reach it*)
  | [h] ->  h :: acc
  | h1 :: (h2 :: _t as t) -> 
      if h1 = h2 then compress_aux acc t
                else compress_aux (h1 :: acc) t

(* 
# compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
- : string list = ["a"; "b"; "c"; "a"; "d"; "e"]         *)
let compress lst = List.rev (compress_aux [] lst)

(** <9> Pack Consecutive Duplicates  
    have two level acc*)
let rec pack_aux cur acc = function
  | [] -> [] (* only [] reach it*)
  | [h] -> (h :: cur) :: acc
  | h1 :: (h2 :: _t as t) ->
      let cur' = h1 :: cur in  
      if h1 = h2 then pack_aux cur' acc t
                else pack_aux [] (cur' :: acc) t

(* 
# pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;
- : string list list =
[["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
  ["e"; "e"; "e"; "e"]]                 *)
let pack lst = List.rev (pack_aux [] [] lst)

(** <10> Run-Length Encoding *)
let rec encode_aux count acc = function
  | [] -> [(0, "")] (* only [] reach it*)
  | [h] -> (count + 1, h) :: acc 
  | h1 :: (h2 :: _t as t) ->
      if h1 = h2 then encode_aux (count + 1) acc t
                 else encode_aux 0 ((count + 1, h1) :: acc) t

(* 
# encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
- : (int * string) list =
[(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")] *)
let encode lst = List.rev (encode_aux 0 [] lst) 


(** <11>  Modified Run-Length Encoding *)
type 'a rle =
  | One of 'a
  | Many of int * 'a
  
let add_1 h = function
  | None -> One h
  | Some (One _) -> Many (2, h)
  | Some (Many (count,c)) -> Many (count + 1, c)
  
let rec encodem_aux r acc = function
  | [] -> [] (* only [] reach it*)
  | [h] -> (add_1 h r) :: acc
  | h1 :: (h2 :: _t as t) -> 
      if h1 = h2 then encodem_aux (Some (add_1 h1 r)) acc t 
                 else encodem_aux None ((add_1 h1 r) :: acc) t

(* 
# encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
- : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
  Many (4, "e")]   *)
let encodem lst = List.rev (encodem_aux None [] lst)



