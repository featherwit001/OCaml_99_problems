open OUnit2
open Ocaml99.H_50to60
open Tests_utilities
open Ocaml99.G_44to49
let example_tree =
  Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)),
       Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)))

let tests_for_50to60  = "tests_for_50to60" >::: [
  make_test_for_equal "count leaves"
  0
  (count_leaves Empty);

  make_test_for_equal "count leaves"
  []
  (leaves Empty);

  make_test_for_equal "internals"
  []
  (internals (Node ('a', Empty, Empty)));

  make_test_for_equal "nodes at level"
  ['b'; 'c']
  (at_level example_tree 2);

  make_test_for_equal "complete binary tree"
  (Node (1, Node (2, Node (4, Empty, Empty), Node (5, Empty, Empty)),
    Node (3, Node (6, Empty, Empty), Empty)))
  (complete_binary_tree [1; 2; 3; 4; 5; 6]);

  make_test_for_equal "layout binary tree"
  (Node (('n', 8, 1),
  Node (('k', 6, 2),
   Node (('c', 2, 3), Node (('a', 1, 4), Empty, Empty),
    Node (('h', 5, 4),
     Node (('g', 4, 5), Node (('e', 3, 6), Empty, Empty), Empty), Empty)),
   Node (('m', 7, 3), Empty, Empty)),
  Node (('u', 12, 2),
   Node (('p', 9, 3), Empty,
    Node (('s', 11, 4), Node (('q', 10, 5), Empty, Empty), Empty)),
   Empty)))
  (layout_binary_tree_1 example_layout_tree1);


  make_test_for_equal "layout 2"
  (Node (('n', 15, 1),
  Node (('k', 7, 2),
   Node (('c', 3, 3), Node (('a', 1, 4), Empty, Empty),
    Node (('e', 5, 4), Node (('d', 4, 5), Empty, Empty),
     Node (('g', 6, 5), Empty, Empty))),
   Node (('m', 11, 3), Empty, Empty)),
  Node (('u', 23, 2),
   Node (('p', 19, 3), Empty, Node (('q', 21, 4), Empty, Empty)), Empty)))
   (layout_binary_tree_2 example_layout_tree2);


  make_test_for_equal "layout 3"
  (Node (('n', 5, 1),
  Node (('k', 3, 2),
   Node (('c', 2, 3), Node (('a', 1, 4), Empty, Empty),
    Node (('h', 3, 4),
     Node (('g', 2, 5), Node (('e', 1, 6), Empty, Empty), Empty), Empty)),
   Node (('m', 4, 3), Empty, Empty)),
  Node (('u', 7, 2),
   Node (('p', 6, 3), Empty,
    Node (('s', 7, 4), Node (('q', 6, 5), Empty, Empty), Empty)),
   Empty)))
  (layout_binary_tree_3 example_layout_tree3)   

]