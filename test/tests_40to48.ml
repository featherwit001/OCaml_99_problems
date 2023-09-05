open OUnit2
open Tests_utilities
open Ocaml99
open F_40to48

let tests_for_40to48 = "tests_for_40to48" >::: [
  make_test_for_equal "table2" 
  [(true, true, true); (true, false, true); (false, true, false);
  (false, false, false)]
  (table2 "a" "b" (And (Var "a", Or (Var "a", Var "b"))));

  make_test_for_equal "table"
  [([("a", true); ("b", true)], true); ([("a", true); ("b", false)], true);
 ([("a", false); ("b", true)], false); ([("a", false); ("b", false)], false)]
  (table ["a"; "b"] (And (Var "a", Or (Var "a", Var "b"))));

  make_test_for_equal "grap 1" ["0"; "1"] (gray 1);
  make_test_for_equal "grap 2" ["00"; "01"; "11"; "10"] (gray 2);
  make_test_for_equal "grap 3" 
  ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"] (gray 3);

  make_test_for_equal "grap_iter 1" ["0"; "1"] (gray_iter 1);
  make_test_for_equal "grap_iter 2" ["00"; "01"; "11"; "10"] (gray_iter 2);
  make_test_for_equal "grap_iter 3" 
  ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"] (gray_iter 3);

  make_test_for_equal "huffman code" 
  [("a", "0"); ("c", "100"); ("b", "101"); ("f", "1100"); ("e", "1101");
 ("d", "111")]
 ([("a", 45); ("b", 13); ("c", 12); ("d", 16);("e", 9); ("f", 5)] 
  |> HuffmanCode.of_list
  |> HuffmanCode.to_list);
]