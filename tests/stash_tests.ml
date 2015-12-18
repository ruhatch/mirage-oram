open Testable

let stash_tests =
    [
      "StashAdd_DummyBlockToEmptyStash_EmptyStash", `Quick,
        (fun _ ->
          let empty = Stash.create () in
          let dummy = OBlock.dummy 1 in
          Stash.add empty dummy;
          Alcotest.(check int) "" 0 (Stash.length empty));
      "StashAdd_ValidBlockToEmptyStash_StashLengthOne", `Quick,
        (fun _ ->
          let empty = Stash.create () in
          let block = (1L, Cstruct.create 1) in
          Stash.add empty block;
          Alcotest.(check int) "" 1 (Stash.length empty));
      "StashFind_ExistsInStash_SomeBlock", `Quick,
        (fun _ ->
          let empty = Stash.create () in
          let block = (1L, Cstruct.create 1) in
          Stash.add empty block;
          Alcotest.(check @@ option oblock) "" (Some block) (Stash.find_index empty 1L));
      "StashFind_NotInStash_None", `Quick,
        (fun _ ->
          let empty = Stash.create () in
          Alcotest.(check @@ option oblock) "" None (Stash.find_index empty 1L));
    ]

let () =
  Alcotest.run "Stash Tests" [
    "Stash Tests", stash_tests
  ]
