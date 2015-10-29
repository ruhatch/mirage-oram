open OUnit2

let oblock_tests =
  "oblock_tests" >:::
    [
      "OBlockToString_ZeroNoBytes_ZeroString" >::
        (fun _ ->
          assert_equal "\000\000\000\000\000\000\000\000" (OBlock.to_string (0L,"")));
      "OBlockToString_OneNoBytes_OneString" >::
        (fun _ ->
          assert_equal "\001\000\000\000\000\000\000\000" (OBlock.to_string (1L,"")));
      "OBlockToString_OneBytes_OneBytesString" >::
        (fun _ ->
          assert_equal "\001\000\000\000\000\000\000\000\001" (OBlock.to_string (1L,"\001")));
      "OBlockOfString_ZeroNoBytes_ZeroString" >::
        (fun _ ->
          assert_equal (0L,"") (OBlock.of_string "\000\000\000\000\000\000\000\000"));
      "OBlockOfString_OneBytes_OneBytesString" >::
        (fun _ ->
          assert_equal (1L,"\001") (OBlock.of_string "\001\000\000\000\000\000\000\000\001"));
    ]

let () =
  run_test_tt_main oblock_tests
