open Alcotest
open Testable
open Lwt

module O = Oram.Make(Block)

module F = Fs.Make(O)

let ( >>= ) x f = x >>= function
  | `Error e -> return (`Error e)
  | `Ok x -> f x

let newFs () =
  Block.connect "disk.img" >>= fun bd ->
  O.connect bd >>= fun bd ->
  F.initialise bd

let newFile (fs : F.t) contents =
  let sectorLength = (String.length contents - 1) / fs.F.info.O.sector_size + 1 in
  let file = Cstruct.create (sectorLength * fs.F.info.O.sector_size) in
  for i = 0 to Cstruct.len file - 1 do
    Cstruct.set_uint8 file i 0
  done;
  Cstruct.blit_from_string contents 0 file 0 (String.length contents);
  file

let oram_fs_tests =
    [
      "FSWriteFile_String_ReadOutString", `Quick,
        (fun () ->
          check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newFs () >>= fun fs ->
              let file = newFile fs "All work and no play makes Dave a dull boy" in
              return (`Ok (file)))
            (fun () ->
              newFs () >>= fun fs ->
              let file = newFile fs "All work and no play makes Dave a dull boy" in
              F.createFile fs "test" >>= fun () ->
              F.writeFile fs "test" file  >>= fun () ->
              F.readFile fs "test"))
    ]

let () =
  Alcotest.run "ORAM FS Tests" [
    "ORAM FS Tests", oram_fs_tests
  ]
