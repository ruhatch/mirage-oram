open Core_kernel.Std
open Alcotest
open Testable
open Generators

let inode_tests =
  [
    "InodeCreate_Ten_Eighty", `Quick,
    (fun () ->
      let inode = Inode.create 10 in
      check int "" 80 (Cstruct.len inode));
    "InodeSetPtr_QuickCheck_GetSamePointer", `Quick,
    (fun () ->
      let inode = Inode.create 100 in
      Quickcheck.test (Quickcheck.Generator.tuple2
                         (indexGenerator 98)
                         pointerGenerator)
                      (fun (index, pointer) ->
                        Inode.setPtr inode index pointer;
                        check int64 "" pointer (Inode.getPtr inode index)));
]

let () =
  Alcotest.run "Inode Tests" [
                 "Inode Tests", inode_tests
               ]
