open Testable

let oblock_tests =
    [
      "OBlockToString_ZeroNoBytes_ZeroString", `Quick,
        (fun _ ->
          Alcotest.(check string) "" "\000\000\000\000\000\000\000\000" (OBlock.to_string (0L,"")));
      "OBlockToString_OneNoBytes_OneString", `Quick,
        (fun _ ->
          Alcotest.(check string) "" "\001\000\000\000\000\000\000\000" (OBlock.to_string (1L,"")));
      "OBlockToString_OneBytes_OneBytesString", `Quick,
        (fun _ ->
          Alcotest.(check string) "" "\001\000\000\000\000\000\000\000\001" (OBlock.to_string (1L,"\001")));
      "OBlockOfString_ZeroNoBytes_ZeroString", `Quick,
        (fun _ ->
          Alcotest.(check oblock) "" (0L,"") (OBlock.of_string "\000\000\000\000\000\000\000\000"));
      "OBlockOfString_OneBytes_OneBytesString", `Quick,
        (fun _ ->
          Alcotest.(check oblock) "" (1L,"\001") (OBlock.of_string "\001\000\000\000\000\000\000\000\001"));
    ]

let () =
  Alcotest.run "OBlock Tests" [
    "OBlock Tests", oblock_tests
  ]
