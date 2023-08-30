open OUnit2
open Ocaml99
open One_to_seven
open Eight_to_thirteen
open Fourteen_to_twenty_one
open Twenty_two_to_thirty


let make_test_for_equal n expect output =
  n >:: (fun _ -> assert_equal expect output)
let make_test_for_raise1 n expect f inputarg1  =
  n >:: (fun _ -> assert_raises (Failure expect) (f inputarg1 )) 
let make_test_for_raise2 n expect f inputarg1 inputarg2 =
  n >:: (fun _ -> assert_raises (Failure expect) (fun _ -> f inputarg1 inputarg2))   

let tests_for_1to7 = "tests_for_1to7" >::: [
  make_test_for_equal "last 1" (Some "d") (last ["a" ; "b" ; "c" ; "d"]);
  make_test_for_equal "last 2" None  (last []);
  make_test_for_equal "last_two 1" (Some ("c", "d")) (last_two ["a"; "b"; "c"; "d"]);
  make_test_for_equal "last_two 2"  None (last_two ["a"]);
  make_test_for_equal "nth 1" "c" (nth ["a"; "b"; "c"; "d"; "e"] 2);
  make_test_for_raise2 "nth fail" "nth" nth ["a"] 2;
  make_test_for_equal "len1" 3 (length ["a"; "b"; "c"]);
  make_test_for_equal "len2" 0 (length []);
  make_test_for_equal "rev" ["c"; "b"; "a"] (rev ["a"; "b"; "c"]);
  make_test_for_equal "is_palindrome" true (is_palindrome ["x"; "a"; "m"; "a"; "x"]);
  make_test_for_equal "is_palindrome" true  (not (is_palindrome ["a"; "b"]));

  make_test_for_equal "flatten" ["a"; "b"; "c"; "d"; "e"] 
    (flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]);
]

let tests_for_8to13 = "tests_for_8to14" >::: [
  make_test_for_equal "compress"  
  ["a"; "b"; "c"; "a"; "d"; "e"]
  (compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]);

  make_test_for_equal "pack" 
  [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]
    (pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]) ;

  make_test_for_equal "encode" 
  [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
  (encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]);

  make_test_for_equal "encodem"  
  [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
  (encodem ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]);

  make_test_for_equal "decode" 
  ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
  (decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]);

  make_test_for_equal "encoded" 
  [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] 
  (encoded ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"])
]

let tests_for_14to22 = "tests_for_14to22" >::: [
  make_test_for_equal "duplicate" 
  ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
  (duplicate ["a"; "b"; "c"; "c"; "d"]);

  make_test_for_equal "replicate"
  ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
  (replicate ["a"; "b"; "c"] 3);

  make_test_for_equal "drop nth"
  ["a"; "b"; "d"; "e"; "f"; "g"; "h"; "i"; "j"]
  (drop_nth ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3);

  make_test_for_equal "drope nth"
  ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
  (drope ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3);

  make_test_for_equal "split 1"
  (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
  (split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3);

  make_test_for_equal "split 2"
  (["a"; "b"; "c"; "d"], [])
  (split ["a"; "b"; "c"; "d"] 5);

  make_test_for_equal "slice"
  ["c"; "d"; "e"; "f"; "g"]
  (slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6);

  make_test_for_equal "slice_easy"
  ["c"; "d"; "e"; "f"; "g"]
  (slice_easy ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6);

  make_test_for_equal "rotate"
  ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
  (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3);

  make_test_for_equal "remove at"
  ["a"; "c"; "d"]
  (remove_at' 1 ["a"; "b"; "c"; "d"]);

  make_test_for_equal "remove at"
  ["a"; "c"; "d"]
  (remove_at 1 ["a"; "b"; "c"; "d"]);

  make_test_for_equal "insert at"
  ["a"; "alfa"; "b"; "c"; "d"]
  (insert_at "alfa" 1 ["a"; "b"; "c"; "d"]);
]


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

let cmp_lists_like_sets lst1 lst2 =
  let uniq1 = List.sort_uniq compare (List.map (List.sort_uniq compare) lst1) in
  let uniq2 = List.sort_uniq compare (List.map (List.sort_uniq compare) lst2) in
   List.length uniq1 = List.length lst1
   &&
   List.length uniq2 = List.length lst2
   &&
   uniq1 = uniq2

(* let make_test_randlist_equal n p expect output =
  n >:: (fun _ -> assert_equal expect output ~printer:p) *)

let make_test_randlist_equal n c expect output =
    n >:: (fun _ -> assert_equal expect output ~cmp:c)  
  
let tests_for_23to30 = "tests_for23to30" >::: [
  make_test_for_equal "range"
  [4; 5; 6; 7; 8; 9]
  (range 4 9);

  make_test_for_equal "select_rand"
  (List.length ["g"; "d"; "a"])
  (List.length (rand_select ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3));

  make_test_for_equal "lotto_select" 
  (List.length [20; 28; 45; 16; 24; 38])
  (List.length (lotto_select 6 49));

  make_test_for_equal "permutation"
  ["c"; "d"; "f"; "e"; "b"; "a"]
  (permutation ["a"; "b"; "c"; "d"; "e"; "f"]);

  make_test_randlist_equal "combination"  cmp_lists_like_sets 
  [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]
  (extract_combination 2 ["a"; "b"; "c"; "d"]);

  make_test_randlist_equal "combination_iter"  cmp_lists_like_sets 
  [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]
  (combination_iter 2 ["a"; "b"; "c"; "d"]);

  make_test_for_equal "combination_iter big"
  184756
  (List.length (combination_iter 10 (range 1 20)))
]


let all_tests = "all tests" >::: [tests_for_1to7; 
                                  tests_for_8to13;
                                  tests_for_14to22;
                                  tests_for_23to30]

let _ = run_test_tt_main all_tests