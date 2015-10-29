open Testable

let posmap_tests = [
    "PosMapIndices_Zero_ZeroZero", `Quick,
      (fun () ->
        Alcotest.(check @@ pair int int) "PosMapIndices_Zero_ZeroZero" (0,0) (PosMap.indices 0L));
    "PosMapIndices_MaxInt_Max32Max32", `Quick,
      (fun () ->
        Alcotest.(check @@ pair int int) "" (2147483647,4294967295) (PosMap.indices Int64.max_int));
    "PosMapIndices_Eighty_ZeroEighty", `Quick,
      (fun () ->
        Alcotest.(check @@ pair int int) "" (0,80) (PosMap.indices 80L));
    "PosMapSet_OutOfBounds_Error", `Quick,
      (fun () ->
        let posmap = PosMap.create 100L in
        Alcotest.check_raises "Out of Bounds" (Invalid_argument "index out of bounds") (fun () -> PosMap.set posmap 100L 1L));
    "PosMapSet_InBounds_ValueSet", `Quick,
      (fun () ->
        let posmap = PosMap.create 100L in
        PosMap.set posmap 1L 100L;
        Alcotest.(check int64) "" (100L) (PosMap.get posmap 1L));
    "PosMapGet_OutOfBounds_Error", `Quick,
      (fun () ->
        let posmap = PosMap.create 100L in
        Alcotest.check_raises "Out of Bounds" (Invalid_argument "index out of bounds") (fun () -> ignore @@ PosMap.get posmap 100L));
    "PosMapLength_LessThanMax32_SameSize", `Quick,
      (fun () ->
        let posmap = PosMap.create 100L in
        Alcotest.(check int64) "" (100L) (PosMap.length posmap));
  ]

let () =
  Alcotest.run "PosMap Tests" [
    "PosMap Tests", posmap_tests
  ]
