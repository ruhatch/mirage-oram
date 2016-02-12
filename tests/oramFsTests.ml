open Alcotest
open Lwt
open Testable

let oram_fs_tests =
  [
    "ORAMFSWriteFile_EmptyString_ReadOutEmptyString", `Slow,
    (fun () ->
      check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.blockDevice "")
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.blockDevice "" >>= fun file ->
              F.writeFile fs "test" file >>= fun () ->
              F.readFile fs "test"));
    "ORAMFSWriteFile_String_ReadOutString", `Slow,
    (fun () ->
      check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.blockDevice "All work and no play makes Dave a dull boy")
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.blockDevice "All work and no play makes Dave a dull boy" >>= fun file ->
              F.writeFile fs "test" file >>= fun () ->
              F.readFile fs "test"));
    "ORAMFSWriteFile_ProjectGutenberg_ReadOutFileCorrectly", `Slow,
    (fun () ->
      let contents = readWholeFile "testFiles/gutenberg/pg61.txt" in
      check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.blockDevice contents)
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.blockDevice contents >>= fun file ->
              F.writeFile fs "pg61.txt" file >>= fun () ->
              F.readFile fs "pg61.txt"));
    "ORAMFSWriteFile_3ProjectGutenbergs_ReadOutFile1Correctly", `Slow,
    (fun () ->
      check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg61.txt"))
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg61.txt") >>= fun file1 ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg62.txt") >>= fun file2 ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg63.txt") >>= fun file3 ->
              F.writeFile fs "pg61.txt" file1 >>= fun () ->
              F.writeFile fs "pg62.txt" file2 >>= fun () ->
              F.writeFile fs "pg63.txt" file3 >>= fun () ->
              F.readFile fs "pg61.txt"));
    "ORAMFSWriteFile_3ProjectGutenbergs_ReadOutFile2Correctly", `Slow,
    (fun () ->
      check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg62.txt"))
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg61.txt") >>= fun file1 ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg62.txt") >>= fun file2 ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg63.txt") >>= fun file3 ->
              F.writeFile fs "pg61.txt" file1 >>= fun () ->
              F.writeFile fs "pg62.txt" file2 >>= fun () ->
              F.writeFile fs "pg63.txt" file3 >>= fun () ->
              F.readFile fs "pg62.txt"));
    "ORAMFSWriteFile_3ProjectGutenbergs_ReadOutFile3Correctly", `Quick,
    (fun () ->
      check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg63.txt"))
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg61.txt") >>= fun file1 ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg62.txt") >>= fun file2 ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg63.txt") >>= fun file3 ->
              F.writeFile fs "pg61.txt" file1 >>= fun () ->
              F.writeFile fs "pg62.txt" file2 >>= fun () ->
              F.writeFile fs "pg63.txt" file3 >>= fun () ->
              F.readFile fs "pg63.txt"));
    "ORAMFSWriteFile_DisconnectWhileWriting_ReadOutFile1Correctly", `Quick,
    (fun () ->
      check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg61.txt"))
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg61.txt") >>= fun file1 ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg62.txt") >>= fun file2 ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg63.txt") >>= fun file3 ->
              F.writeFile fs "pg61.txt" file1 >>= fun () ->
              let%lwt () = O.disconnect fs.F.blockDevice in
              Block.connect "disk.img" >>= fun blockDevice ->
              O.connect blockDevice >>= fun oram ->
              F.connect oram >>= fun fs ->
              F.writeFile fs "pg62.txt" file2 >>= fun () ->
              F.writeFile fs "pg63.txt" file3 >>= fun () ->
              F.readFile fs "pg61.txt"));
    "ORAMFSWriteFile_FullProjectGutenberg_ReadOutFileCorrectly", `Slow,
    (fun () ->
      check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg64.txt"))
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg64.txt") >>= fun file ->
              F.writeFile fs "pg64.txt" file >>= fun () ->
              F.readFile fs "pg64.txt"));
    "ORAMFSWriteFile_WriteLotsOfFiles_ReadRandomOne", `Slow,
    (fun () ->
      let randomFileNum = Random.int 50 in
      check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.blockDevice (readWholeFile (Printf.sprintf "testFiles/random/random%d.txt" randomFileNum)))
            (fun () ->
              newFs () >>= fun fs ->
              let rec loop = function
                | -1 -> return (`Ok ())
                | n ->
                   newFile fs.F.blockDevice (readWholeFile (Printf.sprintf "testFiles/random/random%d.txt" n)) >>= fun file ->
                   F.writeFile fs (Printf.sprintf "random%d.txt" n) file >>= fun () ->
                   loop (n - 1)
              in
              loop 49 >>= fun () ->
              F.readFile fs (Printf.sprintf "random%d.txt" randomFileNum)))
  ]

let () =
  Alcotest.run "ORAM FS Tests" [
                 "ORAM FS Tests", oram_fs_tests
               ]
