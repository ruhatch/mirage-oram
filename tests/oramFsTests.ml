open Alcotest
open Lwt
open Testable

module F = Fs.Make(O)

let newFs () =
  Block.connect "disk.img" >>= fun bd ->
  O.initialise bd >>= fun () ->
  (*let size = Core_kernel.Std.Int64.((pow 2L 14L - 1L) / 2L) in*) (* multiplication by 4 is put into sum and taken out again for 4 times block size... *)
  O.create bd >>= fun bd ->
  F.initialise bd

let oram_fs_tests =
    [
      "ORAMFSWriteFile_EmptyString_ReadOutEmptyString", `Slow,
        (fun () ->
          check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.bd "")
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.bd "" >>= fun file ->
              F.createFile fs "test" >>= fun () ->
              F.writeFile fs "test" file  >>= fun () ->
              F.readFile fs "test"));
      "ORAMFSWriteFile_String_ReadOutString", `Slow,
        (fun () ->
          check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.bd "All work and no play makes Dave a dull boy")
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.bd "All work and no play makes Dave a dull boy" >>= fun file ->
              F.createFile fs "test" >>= fun () ->
              F.writeFile fs "test" file  >>= fun () ->
              F.readFile fs "test"));
      "ORAMFSWriteFile_ProjectGutenburg_ReadOutFileCorrectly", `Slow,
        (fun () ->
          let contents = readWholeFile "testFiles/pg61.txt" in
          check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.bd contents)
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.bd contents >>= fun file ->
              F.createFile fs "pg61.txt" >>= fun () ->
              F.writeFile fs "pg61.txt" file >>= fun () ->
              F.readFile fs "pg61.txt"));
      "ORAMFSWriteFile_3ProjectGutenburgs_ReadOutFile1Correctly", `Slow,
        (fun () ->
          check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.bd (readWholeFile "testFiles/pg61.txt"))
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.bd (readWholeFile "testFiles/pg61.txt") >>= fun file1 ->
              newFile fs.F.bd (readWholeFile "testFiles/pg62.txt") >>= fun file2 ->
              newFile fs.F.bd (readWholeFile "testFiles/pg63.txt") >>= fun file3 ->
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
              newFile fs.F.bd (readWholeFile "testFiles/pg62.txt"))
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.bd (readWholeFile "testFiles/pg61.txt") >>= fun file1 ->
              newFile fs.F.bd (readWholeFile "testFiles/pg62.txt") >>= fun file2 ->
              newFile fs.F.bd (readWholeFile "testFiles/pg63.txt") >>= fun file3 ->
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
              newFile fs.F.bd (readWholeFile "testFiles/pg63.txt"))
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.bd (readWholeFile "testFiles/pg61.txt") >>= fun file1 ->
              newFile fs.F.bd (readWholeFile "testFiles/pg62.txt") >>= fun file2 ->
              newFile fs.F.bd (readWholeFile "testFiles/pg63.txt") >>= fun file3 ->
              F.createFile fs "pg61.txt" >>= fun () ->
              F.writeFile fs "pg61.txt" file1  >>= fun () ->
              F.createFile fs "pg62.txt" >>= fun () ->
              F.writeFile fs "pg62.txt" file2  >>= fun () ->
              F.createFile fs "pg63.txt" >>= fun () ->
              F.writeFile fs "pg63.txt" file3  >>= fun () ->
              F.readFile fs "pg63.txt"));
    ]

let () =
  Alcotest.run "ORAM FS Tests" [
    "ORAM FS Tests", oram_fs_tests
  ]
