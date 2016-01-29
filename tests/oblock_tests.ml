open Alcotest
open Core_kernel.Std
open Testable
open Generators

let oblock_tests =
  [
    "OBlockTest_QuickCheck_OBlocksEqual", `Quick,
    (fun () ->
      Quickcheck.test (oblockGenerator 4096L 512)
                      (fun testOBlock ->
                        check oblock "" testOBlock (OBlock.of_cstruct (OBlock.to_cstruct testOBlock))));
    "OBlockTest_QuickCheck_OBlocksEqual", `Quick,
    (fun () ->
      Quickcheck.test (bucketGenerator 4096L 512 1)
                      (fun testBucket ->
                        check (list oblock) "" testBucket (List.map ~f:(Fn.compose OBlock.of_cstruct OBlock.to_cstruct) testBucket)));
  ]

let () =
  Alcotest.run "OBlock Tests" [
                 "OBlock Tests", oblock_tests
               ]
