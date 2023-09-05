open OUnit2
open Ocaml99
open A_1to7
open B_8to13
open C_14to21

open Tests_utilities

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

let tests_for_8to13 = "tests_for_8to13" >::: [
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

let tests_for_14to21 = "tests_for_14to21" >::: [
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