open Alcotest
open Testable
open Inode

let inode_tests =
    [
      "InodeTypeConv_Ten_MinusTen", `Quick,
        (fun () ->
          check int64 "" (-10L) (typeConv 10L));
      "InodeTypeConv_MinusTen_Ten", `Quick,
        (fun () ->
          check int64 "" 10L (typeConv (-10L)));
      "InodeTypeOfPtr_Indirect_Indirect", `Quick,
        (fun () ->
          match typeOfPtr (-10L) with
            | Direct _ -> fail ""
            | Indirect x -> check int64 "" 10L x);
      "InodeTypeOfPtr_Direct_Direct", `Quick,
        (fun () ->
          match typeOfPtr 10L with
            | Direct x -> check int64 "" 10L x
            | Indirect _ -> fail "");
      "InodeCreate_Ten_Eighty", `Quick,
        (fun () ->
          let inode = create 10 in
          check int "" 80 (Cstruct.len inode));
      "InodeSetPtr_Indirect_Indirect", `Quick,
        (fun () ->
          let inode = create 10 in
          setPtr inode 1 (Indirect 10L);
          match getPtr inode 1 with
            | Direct _ -> fail ""
            | Indirect x -> check int64 "" 10L x);
      "InodeSetPtr_Direct_Direct", `Quick,
        (fun () ->
          let inode = create 10 in
          setPtr inode 1 (Direct 10L);
          match getPtr inode 1 with
            | Direct x -> check int64 "" 10L x
            | Indirect _ -> fail "");
    ]

let () =
  Alcotest.run "Inode Tests" [
    "Inode Tests", inode_tests
  ]
