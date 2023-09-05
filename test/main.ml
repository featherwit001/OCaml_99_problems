open OUnit2
open Tests_1to21
open Tests_22to39
open Tests_40to48
open Tests_heap

let _ = QCheck_runner.run_tests ~verbose:true [qtest_is_mod_floor; qtests_heap]

let all_tests = "all tests" >::: [tests_for_1to7; 
                                  tests_for_8to13;
                                  tests_for_14to21;

                                  tests_for_22to28;
                                  tests_for_29to39;
                                  
                                  tests_for_40to48;
                                  tests_for_heap]

let _ = run_test_tt_main all_tests