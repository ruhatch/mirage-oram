open Alcotest
open Lwt
open Testable

(*let bd =
  match_lwt Block.connect "disk.img" with
    | `Ok bd ->
      begin match_lwt O.connect bd with
        | `Ok bd -> return bd
        | `Error x -> failwith "Failed to connect to ORAM"
      end
    | `Error x -> failwith "Failed to connect to raw Block"*)

module O = Oram.Make(PosMap.InMemory)(Block)

let newORAM () =
  Block.connect "disk.img" >>= fun bd ->
  O.initialise bd >>= fun () ->
  O.create bd

let dummy_bucket =
  newORAM () >>= fun bd ->
    lwt info = O.get_info bd in
      return (`Ok (OBlock.dummy info.O.sector_size,
              OBlock.dummy info.O.sector_size,
              OBlock.dummy info.O.sector_size,
              OBlock.dummy info.O.sector_size))

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
          (check (lwt_t @@ result error bool)) ""
            (fun () -> return (`Ok true))
            (fun () -> newORAM () >>= fun bd ->
              O.read_bucket bd 0L >>= fun bucket ->
              return (`Ok (List.for_all (fun (a,d) -> a = -1L) bucket))));
              (*match bucket with
                | (-1L,_),(-1L,_),(-1L,_),(-1L,_) -> return (`Ok true)
                | (a1,d1),(a2,d2),(a3,d3),(a4,d4) -> Printf.printf "Addresses returned: %Ld %Ld %Ld %Ld\n" a1 a2 a3 a4; return (`Ok false)));*)
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
      "ORAMWriteFile_ProjectGutenburg_ReadOutCorrectly", `Slow,
        (fun () ->
          let contents = readWholeFile "testFiles/pg61.txt" in
          (check (lwt_t @@ result error cstruct)) ""
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
