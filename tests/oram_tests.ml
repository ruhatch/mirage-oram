open Alcotest
open Core_kernel.Std
open Lwt
open Testable
open Generators

let oram_tests =
  [
    "OramFloorLog_One_Zero", `Quick,
    (fun () ->
      check int "" 0 (O.floor_log 1L));
    "OramFloorLog_OneTwentySeven_Six", `Quick,
    (fun () ->
      check int "" 6 (O.floor_log 127L));
    "OramFloorLog_OneTwentyEight_Seven", `Quick,
    (fun () ->
      check int "" 7 (O.floor_log 128L));
    "OramBlockInitialise_Initialised_BlockZeroZero", `Quick,
    (fun () ->
      check (lwt_t @@ result error bool) ""
            (fun () -> return (`Ok true))
            (fun () -> newORAM () >>= fun bd ->
                       O.readBucket bd 0L >>= fun bucket ->
                       return (`Ok (List.for_all ~f:(fun (a,d) -> a = -1L) bucket))));
    "ORAMWriteFile_EmptyString_ReadOutEmptyString", `Quick,
    (fun () ->
      check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newORAM () >>= fun bd ->
              newFile bd "" >>= fun file ->
              return (`Ok (file)))
            (fun () ->
              newORAM () >>= fun bd ->
              newFile bd "" >>= fun file ->
              O.write bd 0L [file] >>= fun () ->
              let buff = Cstruct.create (Cstruct.len file) in
              O.read bd 0L [buff] >>= fun () ->
              return (`Ok buff)));
    "ORAMWriteFile_String_ReadOutString", `Quick,
    (fun () ->
      check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newORAM () >>= fun bd ->
              newFile bd "All work and no play makes Dave a dull boy" >>= fun file ->
              return (`Ok (file)))
            (fun () ->
              newORAM () >>= fun bd ->
              newFile bd "All work and no play makes Dave a dull boy" >>= fun file ->
              O.write bd 0L [file] >>= fun () ->
              let buff = Cstruct.create (Cstruct.len file) in
              O.read bd 0L [buff] >>= fun () ->
              return (`Ok buff)));
    "ORAMWriteFile_ProjectGutenberg_ReadOutCorrectly", `Quick,
    (fun () ->
      let contents = readWholeFile "testFiles/gutenberg/pg61.txt" in
      check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newORAM () >>= fun bd ->
              newFile bd contents >>= fun file ->
              return (`Ok file))
            (fun () ->
              newORAM () >>= fun bd ->
              newFile bd contents >>= fun file ->
              O.write bd 0L [file] >>= fun () ->
              let buff = Cstruct.create (Cstruct.len file) in
              O.read bd 0L [buff] >>= fun () ->
              return (`Ok buff)));
    "ORAMWriteBucket_QuickCheck_ReadSameValueFromBucket", `Slow,
    (fun () ->
      let oram = match Lwt_main.run (newORAM ()) with
        | `Ok oram -> oram
        | `Error _ -> failwith "Failed to connect to ORAM"
      in
      let info = Lwt_main.run (O.get_info oram) in
      Quickcheck.test
        (Quickcheck.Generator.tuple2
           (addressGenerator info.O.size_sectors)
           (bucketGenerator info.O.size_sectors info.O.sector_size 4))
        (fun (address, bucketToWrite) ->
          check (lwt_t @@ result error bucket) ""
                (fun () -> return (`Ok bucketToWrite))
                (fun () ->
                  O.writeBucket oram address bucketToWrite >>= fun () ->
                  O.readBucket oram address)));
    "ORAMWritePathToLeaf_QuickCheck_ReadSamePath", `Slow,
    (fun () ->
      Printf.printf "Testing\n";
      let oram = match Lwt_main.run (newORAM ()) with
        | `Ok oram -> oram
        | `Error _ -> failwith "Failed to connect to ORAM"
      in
      let info = Lwt_main.run (O.get_info oram) in
      let structuralInfo = Lwt_main.run (O.getStructuralInfo oram) in
      Quickcheck.test ~trials:1
                      (Quickcheck.Generator.tuple2 (Quickcheck.Generator.return 0L) (* (leafGenerator structuralInfo.O.numLeaves)*)
                                                   (bucketGenerator info.O.size_sectors info.O.sector_size 4))
                      ~f:(fun (leaf, pathToWrite) ->
                        check (lwt_t @@ result error (list bucket)) ""
                              (fun () -> return (`Ok [pathToWrite]))
                              (fun () ->
                                O.writePathToLeaf oram leaf [pathToWrite] >>= fun () ->
                                O.readPathToLeaf oram leaf)));
    "ORAMAccess_QuickCheck_ReadSameBlock", `Slow,
    (fun () ->
      let oram = match Lwt_main.run (newORAM ()) with
        | `Ok oram -> oram
        | `Error _ -> failwith "Failed to connect to ORAM"
      in
      let info = Lwt_main.run (O.get_info oram) in
      Quickcheck.test (Quickcheck.Generator.tuple2 (addressGenerator info.O.size_sectors) (cstructGenerator info.O.sector_size))
                      (fun (address, data) ->
                        check (lwt_t @@ result error cstruct) ""
                              (fun () -> return (`Ok data))
                              (fun () ->
                                O.access oram O.Write address (Some data) >>= fun _ ->
                                O.access oram O.Read address None)));
    "ORAMGetSet_QuickCheck_GetSamePosition", `Slow,
    (fun () ->
      let oram = match Lwt_main.run (newORAM ()) with
        | `Ok oram -> oram
        | `Error _ -> failwith "Failed to connect to ORAM"
      in
      let info = Lwt_main.run (O.get_info oram) in
      let structuralInfo = Lwt_main.run (O.getStructuralInfo oram) in
      Quickcheck.test (Quickcheck.Generator.tuple2
                         (addressGenerator info.O.size_sectors)
                         (leafGenerator structuralInfo.O.numLeaves))
                      (fun (address, position) ->
                        check (lwt_t @@ result error int64) ""
                              (fun () -> return (`Ok position))
                              (fun () ->
                                O.set oram address position >>= fun () ->
                                O.get oram address)));
  ]

let () =
  Alcotest.run "ORAM Tests" [
                 "ORAM Tests", oram_tests
               ]
