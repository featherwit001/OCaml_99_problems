open OUnit2
open Tests_utilities
open Ocaml99
open F_40to43

let tests_for_40to43 = "tests_for_40to43" >::: [
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

open G_44to49
let bal_trees_height3 =
  [Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
  Node ('x', Empty, Node ('x', Empty, Empty)));
  Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
  Node ('x', Node ('x', Empty, Empty), Empty));
  Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
  Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)));
  Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
  Node ('x', Empty, Node ('x', Empty, Empty)));
  Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
  Node ('x', Node ('x', Empty, Empty), Empty));
  Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
  Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)));
  Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
  Node ('x', Empty, Node ('x', Empty, Empty)));
  Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
  Node ('x', Node ('x', Empty, Empty), Empty));
  Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
  Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)));
  Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
  Node ('x', Empty, Empty));
  Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
  Node ('x', Empty, Empty));
  Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
  Node ('x', Empty, Empty));
  Node ('x', Node ('x', Empty, Empty),
  Node ('x', Empty, Node ('x', Empty, Empty)));
  Node ('x', Node ('x', Empty, Empty),
  Node ('x', Node ('x', Empty, Empty), Empty));
  Node ('x', Node ('x', Empty, Empty),
  Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)))] 

let tests_for_44to49 = "tests_for_44to50" >::: [
  make_test_for_equal "complete binary trees"
  [Node ('x', Node ('x', Empty, Empty),
  Node ('x', Node ('x', Empty, Empty), Empty));

  Node ('x', Node ('x', Empty, Empty),
  Node ('x', Empty, Node ('x', Empty, Empty)));

  Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
  Node ('x', Empty, Empty));

  Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
  Node ('x', Empty, Empty))]
  (cbal_tree 4);

  make_test_for_equal "complete binary tree memoization"
  [Node ('x', Node ('x', Empty, Empty),
  Node ('x', Node ('x', Empty, Empty), Empty));

  Node ('x', Node ('x', Empty, Empty),
  Node ('x', Empty, Node ('x', Empty, Empty)));

  Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
  Node ('x', Empty, Empty));

  Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
  Node ('x', Empty, Empty))]
  (cbal_trees_memo_wrap 4);

  make_test_for_equal "construct bst"
  (Node (3, 
    Node (2, 
      Node (1, Empty, Empty), 
      Empty),
    Node (5, 
      Empty, 
      Node (7, Empty, Empty))))
  (construct [3; 2; 5; 7; 1]);

  make_test_for_equal "is_symmetric bst"
  true
  (is_symmetric (construct [5; 3; 18; 1; 4; 12; 21]));

  make_test_for_equal "is_symmetric bst 2"
  false
  (is_symmetric (construct [3; 2; 5; 7; 4]));


  make_test_for_equal "sym_cbal_trees 1"
  [ Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
    Node ('x', Empty, Node ('x', Empty, Empty)));
    Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
    Node ('x', Node ('x', Empty, Empty), Empty))]
  (sym_cbal_trees 5);

  make_test_for_equal "sym_cal_trees 2"
  256
  (List.length (sym_cbal_trees 57));

  make_test_for_equal "hbal_trees"
  bal_trees_height3
  (hbal_tree 3);

  make_test_for_equal "hbal_trees_memo"
  bal_trees_height3
  (hbal_tree_memo 3);

  make_test_for_equal "hbal_tree_nodes"
  1553
  ( List.length (hbal_tree_nodes 15));
]


let gen_int = QCheck.Gen.(nat)
let arb_gen = QCheck.make gen_int
let is_right_min_height n =
  min_height_formula n =  min_height_memo n
let qtest_is_right_min_height = QCheck.Test.make ~name:"min_height_of_n_nodes_in_bal_bin_tree"
                     ~count:100000 arb_gen is_right_min_height 
                

let is_right_max_height n =                      
  max_height n = max_height_optimization n
let qtest_is_right_max_height = QCheck.Test.make ~name:"min_height_of_n_nodes_in_bal_bin_tree"
  ~count:100000 arb_gen is_right_max_height