open OUnit2

let make_test_for_equal n expect output =
  n >:: (fun _ -> assert_equal expect output)
let make_test_for_raise1 n expect f inputarg1  =
  n >:: (fun _ -> assert_raises (Failure expect) (f inputarg1 )) 
let make_test_for_raise2 n expect f inputarg1 inputarg2 =
  n >:: (fun _ -> assert_raises (Failure expect) (fun _ -> f inputarg1 inputarg2))   

let pp_list pp_elt lst = 
  let pp_elts lst = 
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ (pp_elt h)
      | h1 ::(_h2 :: _t as t) ->
        if n = 100 then acc ^ "......"
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t
      in 
      loop 0 "" lst
    in
    "[" ^ (pp_elts lst) ^ "]"

let pp_intlst = pp_list string_of_int

let cmp_llists_like_sets lst1 lst2 =
  let uniq1 = List.sort_uniq compare (List.map (List.sort_uniq compare) lst1) in
  let uniq2 = List.sort_uniq compare (List.map (List.sort_uniq compare) lst2) in
    List.length uniq1 = List.length lst1
    &&
    List.length uniq2 = List.length lst2
    &&
    uniq1 = uniq2


let cmp_lllists_like_sets lll1 lll2 =
  let uniq1 = List.sort_uniq compare (
                List.map (fun ll -> 
                  List.sort_uniq compare 
                    (List.map (fun l -> List.sort_uniq compare l) ll)) lll1) in
  let uniq2 =  List.sort_uniq compare (
                  List.map (fun ll -> 
                    List.sort_uniq compare 
                    (List.map (fun l -> List.sort_uniq compare l) ll)) lll2) in
    List.length uniq1 = List.length lll1
    &&
    List.length uniq2 = List.length lll2
    &&
    uniq1 = uniq2


(* let make_test_randlist_equal n p expect output =
  n >:: (fun _ -> assert_equal expect output ~printer:p) *)

let make_test_randlist_equal n c expect output =
  n >:: (fun _ -> assert_equal expect output ~cmp:c)     

