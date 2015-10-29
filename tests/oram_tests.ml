(*module Testing = Oram.Make(Block)*)

open OUnit2

let oram_tests =
  "oram_tests" >:::
    [
      (*"OramFloorLog_One_Zero" >::
        (fun _ ->
          assert_equal 0 0)*)
    ]

let () =
  run_test_tt_main oram_tests
