open Alcotest
open Testable
open Inode

let inode_tests =
    [
      "InodeCreate_Ten_Eighty", `Quick,
        (fun () ->
          let inode = create 10 in
          check int "" 80 (Cstruct.len inode));
    ]

let () =
  Alcotest.run "Inode Tests" [
    "Inode Tests", inode_tests
  ]
