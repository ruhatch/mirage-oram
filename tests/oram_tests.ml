open Alcotest
open Testable
open Lwt

module O = Oram.Make(Block)

let bd =
  match_lwt Block.connect "disk.img" with
    | `Ok bd ->
      begin match_lwt O.connect bd with
        | `Ok bd -> return bd
        | `Error x -> failwith "Failed to connect to ORAM"
      end
    | `Error x -> failwith "Failed to connect to raw Block"

let dummy_bucket =
  bd >>= fun bd ->
    lwt info = O.get_info bd in
      return (OBlock.dummy info.O.sector_size,
              OBlock.dummy info.O.sector_size,
              OBlock.dummy info.O.sector_size,
              OBlock.dummy info.O.sector_size)

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
          (check (lwt_t bool)) ""
            (fun () -> return_true)
            (fun () -> bd >>= fun bd ->
              match_lwt O.read_bucket bd 0L with
                | `Ok ((-1L,_),(-1L,_),(-1L,_),(-1L,_)) -> return_true
                | `Ok _ -> return_false
                | `Error x -> return_false))
    ]

let () =
  Alcotest.run "ORAM Tests" [
    "ORAM Tests", oram_tests
  ]
