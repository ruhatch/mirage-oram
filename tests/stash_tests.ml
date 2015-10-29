open OUnit2

let stash_tests =
  "stash_tests" >:::
    [
      "StashAdd_DummyBlockToEmptyStash_EmptyStash" >::
        (fun _ ->
          let empty = Stash.create () in
          let dummy = OBlock.dummy 1 in
          Stash.add empty dummy;
          assert_equal 0 (Stash.length empty));
      "StashAdd_ValidBlockToEmptyStash_StashLengthOne" >::
        (fun _ ->
          let empty = Stash.create () in
          let block = (1L, Bytes.create 1) in
          Stash.add empty block;
          assert_equal 1 (Stash.length empty));
      "StashFind_ExistsInStash_SomeBlock" >::
        (fun _ ->
          let empty = Stash.create () in
          let block = (1L, Bytes.create 1) in
          Stash.add empty block;
          assert_equal (Some block) (Stash.find_index empty 1L));
      "StashFind_NotInStash_None" >::
        (fun _ ->
          let empty = Stash.create () in
          assert_equal None (Stash.find_index empty 1L));
    ]

let () =
  run_test_tt_main stash_tests
