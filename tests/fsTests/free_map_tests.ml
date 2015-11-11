open Alcotest
open Testable

let free_map_tests =
    [
      "FreeMapCreate_Ten_ZeroFalse", `Quick,
        (fun () ->
          let freeMap = FreeMap.create 10 in
          check bool "" false (FreeMap.get freeMap 0));
      "FreeMapCreate_Ten_OneFalse", `Quick,
        (fun () ->
          let freeMap = FreeMap.create 10 in
          check bool "" false (FreeMap.get freeMap 1));
      "FreeMapCreate_Ten_TwoFalse", `Quick,
        (fun () ->
          let freeMap = FreeMap.create 10 in
          check bool "" false (FreeMap.get freeMap 2));
      "FreeMapCreate_Ten_ThreeTrue", `Quick,
        (fun () ->
          let freeMap = FreeMap.create 10 in
          check bool "" true (FreeMap.get freeMap 3));
      "FreeMapAlloc_One_SingletonListOfThree", `Quick,
        (fun () ->
          let freeMap = FreeMap.create 10 in
          check (list int) "" [3] (FreeMap.alloc freeMap 1));
      "FreeMapAlloc_Two_ListOfThreeAndFour", `Quick,
        (fun () ->
          let freeMap = FreeMap.create 10 in
          check (list int) "" [3;4] (FreeMap.alloc freeMap 2));
      "FreeMapAlloc_One_AllocatedThree", `Quick,
        (fun () ->
          let freeMap = FreeMap.create 10 in
          ignore (FreeMap.alloc freeMap 1);
          check bool "" false (FreeMap.get freeMap 3));
    ]

let () =
  Alcotest.run "FreeMap Tests" [
    "FreeMap Tests", free_map_tests
  ]
