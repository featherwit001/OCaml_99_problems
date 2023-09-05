open OUnit2
open Tests_utilities
open Ocaml99
open D_22to28
open E_29to39



let tests_for_22to28 = "tests_for22to28" >::: [
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

  make_test_randlist_equal "combination"  cmp_llists_like_sets 
  [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]
  (extract_combination 2 ["a"; "b"; "c"; "d"]);

  make_test_randlist_equal "combination_iter"  cmp_llists_like_sets 
  [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]
  (combination_iter 2 ["a"; "b"; "c"; "d"]);

  make_test_for_equal "combination_iter big"
  184756
  (List.length (combination_iter 10 (range 1 20)));

  make_test_randlist_equal "group" cmp_lllists_like_sets
  [[["a"; "b"]; ["c"]]; [["a"; "c"]; ["b"]]; [["b"; "c"]; ["a"]];
 [["a"; "b"]; ["d"]]; [["a"; "c"]; ["d"]]; [["b"; "c"]; ["d"]];
 [["a"; "d"]; ["b"]]; [["b"; "d"]; ["a"]]; [["a"; "d"]; ["c"]];
 [["b"; "d"]; ["c"]]; [["c"; "d"]; ["a"]]; [["c"; "d"]; ["b"]]]
 (group ["a"; "b"; "c"; "d"] [2; 1]);


 make_test_randlist_equal "group" cmp_lllists_like_sets
  [[["a"; "b"]; ["c"]]; [["a"; "c"]; ["b"]]; [["b"; "c"]; ["a"]];
 [["a"; "b"]; ["d"]]; [["a"; "c"]; ["d"]]; [["b"; "c"]; ["d"]];
 [["a"; "d"]; ["b"]]; [["b"; "d"]; ["a"]]; [["a"; "d"]; ["c"]];
 [["b"; "d"]; ["c"]]; [["c"; "d"]; ["a"]]; [["c"; "d"]; ["b"]]]
 (group_iter ["a"; "b"; "c"; "d"] [2; 1]);

  make_test_for_equal "length_sort" 
  [["o"]; ["d"; "e"]; ["d"; "e"]; ["m"; "n"]; ["a"; "b"; "c"]; ["f"; "g"; "h"];
 ["i"; "j"; "k"; "l"]]
  (length_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"];
  ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]]);

  make_test_for_equal "frequency_sort"
  [["i"; "j"; "k"; "l"]; ["o"]; ["a"; "b"; "c"]; ["f"; "g"; "h"]; ["d"; "e"];
 ["d"; "e"]; ["m"; "n"]]
  (frequency_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"];
  ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]]);
]


let gen_tup_int = QCheck.Gen.(tup2 int int)
let arb_gen_tup = QCheck.make gen_tup_int 
let is_mod_floor (a, b) =
  a mod b = a - b * int_of_float (floor (float_of_int a) /. (float_of_int b))
let qtest_is_mod_floor = QCheck.Test.make ~name:"is_mod_floor" ~count:100
                            arb_gen_tup is_mod_floor


let tests_for_29to39 = "tests_for_29to39" >::: [
  make_test_for_equal "is_prime1" true (is_prime 7);
  make_test_for_equal "is_prime2" false (is_prime 1);
  make_test_for_equal "is_prime3" false (is_prime 12);
  make_test_for_equal "gcd" 1 (gcd 13 27);
  make_test_for_equal "totient function 1" 4 (phi 10);
  make_test_for_equal "totient function 2" 12 (phi 13);
  make_test_for_equal "totient function 2" 1 (phi 1);
  make_test_for_equal "factor1"  [3; 3; 5; 7] (factors 315);
  make_test_for_equal "factor2"  [(3, 2); (5, 1); (7, 1)] (factors2 315);
  make_test_for_equal "factor2" [(2, 1); (5, 1)] (factors2 10);
  make_test_for_equal "totient function improved " 4 (phi_improved 10);
  make_test_for_equal "totient function improved " 12 (phi_improved 13);
  make_test_for_equal "totient function improved " 1 (phi_improved 1);
  make_test_for_equal "all_primes" 1000 (List.length (all_primes 2 7920));
  make_test_for_equal "goldbach" (5, 23) (goldbach 28);

  make_test_for_equal "goldbach_list" 
  [(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13));(20, (3, 17))]
  (goldbach_list 9 20);
]