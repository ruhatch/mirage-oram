module O = Oram.Make(Block)

let oram_tests =
    [
      "OramFloorLog_One_Zero", `Quick,
        (fun _ ->
          Alcotest.(check int) "OramFloorLog_One_Zero" 0 (O.floor_log 1L))
    ]

let () =
  Alcotest.run "ORAM Tests" [
    "ORAM Tests", oram_tests
  ]
