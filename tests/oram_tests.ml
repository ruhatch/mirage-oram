open Alcotest
open Lwt
open Testable

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
    "OramBlockInitialise_Initialised_BlockZeroZero", `Slow,
    (fun () ->
      check (lwt_t @@ result error bool) ""
            (fun () -> return (`Ok true))
            (fun () -> newORAM () >>= fun bd ->
                       O.readBucket bd 0L >>= fun bucket ->
                       return (`Ok (List.for_all (fun (a,d) -> a = -1L) bucket))));
    "ORAMWriteFile_EmptyString_ReadOutEmptyString", `Slow,
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
    "ORAMWriteFile_String_ReadOutString", `Slow,
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
    "ORAMWriteFile_ProjectGutenberg_ReadOutCorrectly", `Slow,
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
  ]

let () =
  Alcotest.run "ORAM Tests" [
                 "ORAM Tests", oram_tests
               ]
