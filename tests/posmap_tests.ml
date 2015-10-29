open OUnit2

let posmap_tests =
  "posmap_tests" >:::
    [
      "PosMapIndices_Zero_ZeroZero" >::
        (fun _ ->
          assert_equal (0,0) (PosMap.indices 0L));
      "PosMapIndices_MaxInt_Max32Max32" >::
        (fun _ ->
          assert_equal (2147483647,4294967295) (PosMap.indices Int64.max_int));
      "PosMapIndices_Eighty_ZeroEighty" >::
        (fun _ ->
          assert_equal (0,80) (PosMap.indices 80L));
      "PosMapSet_OutOfBounds_Error" >::
        (fun _ ->
          let posmap = PosMap.create 100L in
          assert_raises (Invalid_argument "index out of bounds") (fun () -> PosMap.set posmap 100L 1L));
      "PosMapSet_InBounds_ValueSet" >::
        (fun _ ->
          let posmap = PosMap.create 100L in
          PosMap.set posmap 1L 100L;
          assert_equal (100L) (PosMap.get posmap 1L));
      "PosMapGet_OutOfBounds_Error" >::
        (fun _ ->
          let posmap = PosMap.create 100L in
          assert_raises (Invalid_argument "index out of bounds") (fun () -> PosMap.get posmap 100L));
      "PosMapLength_LessThanMax32_SameSize" >::
        (fun _ ->
          let posmap = PosMap.create 100L in
          assert_equal (100L) (PosMap.length posmap));
    ]

let () =
  run_test_tt_main posmap_tests
