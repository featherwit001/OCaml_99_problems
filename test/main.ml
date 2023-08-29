open OUnit2
open Ocaml99.One_to_seven
open Ocaml99.Eight_to_fourteen

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


let tests_for_8to14 = "tests_for_8to14" >::: [
  make_test_for_equal "compress"  ["a"; "b"; "c"; "a"; "d"; "e"]
  (compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]);
  make_test_for_equal "pack" 
  [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]
    (pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]) ;
  make_test_for_equal "encode" [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
  (encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]);
  make_test_for_equal "encodem"  
  [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
  (encodem ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"])
]

let all_tests = "all tests" >::: [tests_for_1to7; tests_for_8to14]

let _ = run_test_tt_main all_tests