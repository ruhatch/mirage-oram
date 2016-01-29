open Core_kernel.Std
open Alcotest
open Testable
open Generators

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
    "StashAdd_QuickCheck_FindAfterAdd", `Quick,
    (fun () ->
      let empty = Stash.create () in
      Quickcheck.test (Quickcheck.Generator.tuple2
                         (addressGenerator 500L)
                         (cstructGenerator 100))
                      (fun (address, data) ->
                        Stash.add empty ~address ~data;
                        check (option cstruct) ""
                              (Some data)
                              (Stash.find empty address)));
    "StashRemove_QuickCheck_None", `Quick,
    (fun () ->
      let empty = Stash.create () in
      Quickcheck.test (Quickcheck.Generator.tuple2
                         (addressGenerator 500L)
                         (cstructGenerator 100))
                      (fun (address, data) ->
                        Stash.add empty ~address ~data;
                        Stash.remove empty address;
                        check (option cstruct) ""
                              None
                              (Stash.find empty address)));
  ]

let () =
  Alcotest.run "Stash Tests" [
                 "Stash Tests", stash_tests
               ]
