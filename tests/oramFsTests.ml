open Alcotest
open Testable
open Lwt

module O = Oram.Make(Oram.Make(PosMap.InMemory))(Block)

module F = Fs.Make(O)

let ( >>= ) x f = x >>= function
  | `Error e -> return (`Error e)
  | `Ok x -> f x

let newFs () =
  Block.connect "disk.img" >>= fun bd ->
  O.initialise bd >>= fun () ->
  let size = Core_kernel.Std.Int64.(pow 2L 16L - 4L) in (* multiplication by 4 is put into sum *)
  O.create ~size bd >>= fun bd ->
  F.initialise bd

let newFile (fs : F.t) contents =
  let sectorLength = (String.length contents - 1) / fs.F.info.O.sector_size + 1 in
  let file = Cstruct.create (sectorLength * fs.F.info.O.sector_size) in
  for i = 0 to Cstruct.len file - 1 do
    Cstruct.set_uint8 file i 0
  done;
  Cstruct.blit_from_string contents 0 file 0 (String.length contents);
  file

let readWholeFile name =
  let file = Unix.openfile name [Unix.O_RDWR; Unix.O_CREAT] 0o664 in
  let length = (Unix.fstat file).Unix.st_size in
  let buff = Bytes.create length in
  let rec loop off = function
    | 0 -> buff
    | n ->
      let read = Unix.read file buff off n in
      loop (off + read) (n - read)
  in loop 0 length

let writeWholeFile name contents =
  let file = Unix.openfile name [Unix.O_RDWR; Unix.O_CREAT] 0o664 in
  let length = Cstruct.len contents in
  let buff = Bytes.create length in
  Cstruct.blit_to_string contents 0 buff 0 length;
  let rec loop off = function
    | 0 -> ()
    | n ->
      let written = Unix.write file buff off n in
      loop (off + written) (n - written)
  in loop 0 length

let oram_fs_tests =
    [
      "ORAMFSWriteFile_EmptyString_ReadOutEmptyString", `Slow,
        (fun () ->
          check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newFs () >>= fun fs ->
              let file = newFile fs "" in
              return (`Ok (file)))
            (fun () ->
              newFs () >>= fun fs ->
              let file = newFile fs "" in
              F.createFile fs "test" >>= fun () ->
              F.writeFile fs "test" file  >>= fun () ->
              F.readFile fs "test"));
      "ORAMFSWriteFile_String_ReadOutString", `Slow,
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
              F.readFile fs "test"));
      "ORAMFSWriteFile_ProjectGutenburg_ReadOutFileCorrectly", `Slow,
        (fun () ->
          let contents = readWholeFile "testFiles/pg61.txt" in
          check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newFs () >>= fun fs ->
              let file = newFile fs contents in
              writeWholeFile "pg61.input" file;
              return (`Ok (file)))
            (fun () ->
              newFs () >>= fun fs ->
              let file = newFile fs contents in
              F.createFile fs "pg61.txt" >>= fun () ->
              F.writeFile fs "pg61.txt" file >>= fun () ->
              F.readFile fs "pg61.txt" >>= fun returned ->
              writeWholeFile "pg61.output" returned;
              return (`Ok returned)));
      (*"ORAMFSWriteFile_3ProjectGutenburgs_ReadOutFile1Correctly", `Slow,
        (fun () ->
          check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newFs () >>= fun fs ->
              let file1 = newFile fs (readWholeFile "testFiles/pg61.txt") in
              return (`Ok (file1)))
            (fun () ->
              newFs () >>= fun fs ->
              let file1 = newFile fs (readWholeFile "testFiles/pg61.txt") in
              let file2 = newFile fs (readWholeFile "testFiles/pg62.txt") in
              let file3 = newFile fs (readWholeFile "testFiles/pg63.txt") in
              F.createFile fs "pg61.txt" >>= fun () ->
              F.writeFile fs "pg61.txt" file1  >>= fun () ->
              F.createFile fs "pg62.txt" >>= fun () ->
              F.writeFile fs "pg62.txt" file2  >>= fun () ->
              F.createFile fs "pg63.txt" >>= fun () ->
              F.writeFile fs "pg63.txt" file3  >>= fun () ->
              F.readFile fs "pg61.txt"));
      "ORAMFSWriteFile_3ProjectGutenburgs_ReadOutFile2Correctly", `Slow,
        (fun () ->
          check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newFs () >>= fun fs ->
              let file2 = newFile fs (readWholeFile "testFiles/pg62.txt") in
              return (`Ok (file2)))
            (fun () ->
              newFs () >>= fun fs ->
              let file1 = newFile fs (readWholeFile "testFiles/pg61.txt") in
              let file2 = newFile fs (readWholeFile "testFiles/pg62.txt") in
              let file3 = newFile fs (readWholeFile "testFiles/pg63.txt") in
              F.createFile fs "pg61.txt" >>= fun () ->
              F.writeFile fs "pg61.txt" file1  >>= fun () ->
              F.createFile fs "pg62.txt" >>= fun () ->
              F.writeFile fs "pg62.txt" file2  >>= fun () ->
              F.createFile fs "pg63.txt" >>= fun () ->
              F.writeFile fs "pg63.txt" file3  >>= fun () ->
              F.readFile fs "pg62.txt"));
      "ORAMFSWriteFile_3ProjectGutenburgs_ReadOutFile3Correctly", `Slow,
        (fun () ->
          check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newFs () >>= fun fs ->
              let file3 = newFile fs (readWholeFile "testFiles/pg63.txt") in
              return (`Ok (file3)))
            (fun () ->
              newFs () >>= fun fs ->
              let file1 = newFile fs (readWholeFile "testFiles/pg61.txt") in
              let file2 = newFile fs (readWholeFile "testFiles/pg62.txt") in
              let file3 = newFile fs (readWholeFile "testFiles/pg63.txt") in
              F.createFile fs "pg61.txt" >>= fun () ->
              F.writeFile fs "pg61.txt" file1  >>= fun () ->
              F.createFile fs "pg62.txt" >>= fun () ->
              F.writeFile fs "pg62.txt" file2  >>= fun () ->
              F.createFile fs "pg63.txt" >>= fun () ->
              F.writeFile fs "pg63.txt" file3  >>= fun () ->
              F.readFile fs "pg63.txt"));*)
    ]

let () =
  Alcotest.run "ORAM FS Tests" [
    "ORAM FS Tests", oram_fs_tests
  ]
