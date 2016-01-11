open Alcotest
open Testable

let free_map_tests =
    [
      "FreeMapCreate_OneTen_ZeroFalse", `Quick,
        (fun () ->
          let freeMap = FreeMap.create 1 10 in
          check bool "" false (FreeMap.get freeMap 0));
      "FreeMapCreate_OneTen_OneFalse", `Quick,
        (fun () ->
          let freeMap = FreeMap.create 1 10 in
          check bool "" false (FreeMap.get freeMap 1));
      "FreeMapCreate_OneTen_TwoTrue", `Quick,
        (fun () ->
          let freeMap = FreeMap.create 1 10 in
          check bool "" true (FreeMap.get freeMap 2));
      "FreeMapCreate_TwoTen_TwoFalse", `Quick,
        (fun () ->
          let freeMap = FreeMap.create 2 10 in
          check bool "" false (FreeMap.get freeMap 2));
      "FreeMapCreate_TwoTen_ThreeTrue", `Quick,
        (fun () ->
          let freeMap = FreeMap.create 2 10 in
          check bool "" true (FreeMap.get freeMap 3));
      "FreeMapAlloc_One_SingletonListOfTwo", `Quick,
        (fun () ->
          let freeMap = FreeMap.create 1 10 in
          check (list int64) "" [2L] (FreeMap.alloc freeMap 1));
      "FreeMapAlloc_Two_ListOfTwoAndThree", `Quick,
        (fun () ->
          let freeMap = FreeMap.create 1 10 in
          check (list int64) "" [2L;3L] (FreeMap.alloc freeMap 2));
      "FreeMapAlloc_One_AllocatedTwo", `Quick,
        (fun () ->
          let freeMap = FreeMap.create 1 10 in
          ignore (FreeMap.alloc freeMap 1);
          check bool "" false (FreeMap.get freeMap 2));
    ]

let () =
  Alcotest.run "FreeMap Tests" [
    "FreeMap Tests", free_map_tests
  ]
