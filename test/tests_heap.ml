open OUnit2
open Tests_utilities

open Ocaml99
open Heap
let max_len = 1000
let max_value = max_len / 2
let min_value = - max_value
let gen_list_len = QCheck.Gen.(int_range 10 max_len)
let gen_list_boundary = QCheck.Gen.(int_range min_value max_value)
let gen_list_int = QCheck.Gen.(list_size gen_list_len gen_list_boundary)

module MinHeap = Make(Int)
open MinHeap

let tests_heap lst =
  let len = List.length lst in 
  let mh = of_list lst in
  for _ = 1 to (len / 2) do
    ignore(pop mh)
  done;
  mh |> to_list |> is_order


let arb_list_int = QCheck.make gen_list_int
let qtests_heap = QCheck.Test.make ~name:"qtests_heap" ~count:100 
                         arb_list_int tests_heap


let tests_for_heap = "tests_for_heap" >::: [
  make_test_for_equal "is_empty" true (is_empty (create ()));
  
  make_test_for_equal "push 1 pop 1" (Some 1) 
  (let minheap = create () in 
    push 1 minheap;
    pop minheap );
  
  make_test_for_equal "is_empty" true 
  (let minheap = create () in 
     push 1 minheap;
     ignore(pop minheap);
     is_empty minheap);
  
  make_test_for_equal "heap order1" 
  [1; 2; 3;]
  ([2; 3; 1] |> of_list |> to_list);

  make_test_for_equal "heap order2" 
  [1; 2; 3; 4]
  ([2; 3; 4; 1;] |> of_list |> to_list);

  make_test_for_equal "heap ordermh3" 
  [1; 2; 3; 4;5;6;7;8;9]
  ([3;6;9;8;5;4;1;7;2] |> of_list |> to_list); 

  make_test_for_equal "heap order3" 
  [1; 2; 3; 4;5;6;7;8;9;10;11;12;13;14;15;16;17]
  ([13;14; 4;5;10;8;9;15;6;7; 2; 12;17;3;11;16;1;] |> of_list |> to_list);
]



  

 