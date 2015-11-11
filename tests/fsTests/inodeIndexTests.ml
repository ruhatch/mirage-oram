open Alcotest
open Testable
open InodeIndex

let inode_index_tests =
    [
      "InodeIndexCreate_Ten_Eighty", `Quick,
        (fun () ->
          let inodeIndex = create 10 in
          check int "" 80 (Cstruct.len inodeIndex));
      "InodeIndexSet_OneFifty_OneFifty", `Quick,
        (fun () ->
          let inodeIndex = create 10 in
          set inodeIndex 1 150L;
          check int64 "" 150L (get inodeIndex 1));
      "InodeIndexNextFree_SetZeroOneThree_Two", `Quick,
        (fun () ->
          let inodeIndex = create 10 in
          set inodeIndex 0 10L;
          set inodeIndex 1 14L;
          set inodeIndex 3 120L;
          check int "" 2 (nextFree inodeIndex));
      "InodeIndexNextFree_AllFull_Fail", `Quick,
        (fun () ->
          let inodeIndex = create 1 in
          set inodeIndex 0 10L;
          check_raises "" (Failure "No available inodes") (fun () -> ignore (nextFree inodeIndex)));
    ]

let () =
  Alcotest.run "Inode Index Tests" [
    "Inode Index Tests", inode_index_tests
  ]
