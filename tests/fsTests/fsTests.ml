open Alcotest
open Testable
open Fs
open Lwt

module F = Make(Block)

let ( >>= ) x f = x >>= function
  | `Error e -> return (`Error e)
  | `Ok x -> f x

let newFs () =
  Block.connect "disk.img" >>= fun bd ->
  F.initialise bd

let newFile (fs : F.t) contents =
  let sectorLength = (String.length contents - 1) / fs.F.info.Block.sector_size + 1 in
  let file = Cstruct.create (sectorLength * fs.F.info.Block.sector_size) in
  for i = 0 to Cstruct.len file - 1 do
    Cstruct.set_uint8 file i 0
  done;
  Cstruct.blit_from_string contents 0 file 0 (String.length contents);
  file

(* Some of these tests are kind of random and should really be located in other places *)

let fs_tests =
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
              F.writeFile fs "test" file  >>= fun () ->
              F.readFile fs "test"))
    ]

let () =
  Alcotest.run "FS Tests" [
    "FS Tests", fs_tests
  ]
