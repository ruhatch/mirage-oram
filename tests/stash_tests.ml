open Testable

let stash_tests =
    [
      "StashAdd_DummyBlockToEmptyStash_EmptyStash", `Quick,
        (fun _ ->
          let empty = Stash.create () in
          match OBlock.dummy 1 with
            | (address, data) -> Stash.add empty ~address ~data;
          Alcotest.(check int) "" 0 (Stash.length empty));
      "StashAdd_ValidBlockToEmptyStash_StashLengthOne", `Quick,
        (fun _ ->
          let empty = Stash.create () in
          Stash.add empty ~address:1L ~data:(Cstruct.create 1);
          Alcotest.(check int) "" 1 (Stash.length empty));
      "StashFind_ExistsInStash_SomeBlock", `Quick,
        (fun _ ->
          let empty = Stash.create () in
          let data = Cstruct.create 1 in
          Stash.add empty ~address:1L ~data;
          Alcotest.(check @@ option cstruct) "" (Some data) (Stash.find empty 1L));
      "StashFind_NotInStash_None", `Quick,
        (fun _ ->
          let empty = Stash.create () in
          Alcotest.(check @@ option cstruct) "" None (Stash.find empty 1L));
    ]

let () =
  Alcotest.run "Stash Tests" [
    "Stash Tests", stash_tests
  ]
