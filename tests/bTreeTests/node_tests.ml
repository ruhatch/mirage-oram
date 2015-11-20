open Alcotest
open Testable
open Node

let node_tests =
    [
      "NodeCreate_512_MinDegree14", `Quick,
        (fun () ->
          let node = Node.create 512 in
          check int "" 14 (Node.minDegree node));
      "NodeCreate_512_NoKeys0", `Quick,
        (fun () ->
          let node = Node.create 512 in
          check int "" 0 (Node.noKeys node));
      "NodeCreate_512_PageSize512", `Quick,
        (fun () ->
          let node = Node.create 512 in
          check int "" 512 (Node.pageSize node));
      "NodeSetLeaf_True_LeafTrue", `Quick,
        (fun () ->
          let node = Node.create 512 in
          Node.setLeaf node true;
          check bool "" true (Node.leaf node));
      "NodeSetChild_100_100", `Quick,
        (fun () ->
          let node = Node.create 512 in
          Node.setChild node 1 100L;
          check int64 "" 100L (Node.getChild node 1));
      "NodeSetKey_100_100", `Quick,
        (fun () ->
          let node = Node.create 512 in
          Node.setKey node 1 100;
          check int "" 100 (Node.getKey node 1));
      "NodeSetValue_100_100", `Quick,
        (fun () ->
          let node = Node.create 512 in
          Node.setValue node 1 100L;
          check int64 "" 100L (Node.getValue node 1));
    ]

let () =
  Alcotest.run "Node Tests" [
    "Node Tests", node_tests
  ]
