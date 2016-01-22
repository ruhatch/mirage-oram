open Alcotest
open Lwt
open Testable

(* module F = Fs.Make(Block)

let newFile blockDevice contents =
  lwt info = Block.get_info blockDevice in
  let sectorLength = (String.length contents - 1) / info.Block.sector_size + 1 in
  let file = Cstruct.create (sectorLength * info.Block.sector_size) in
  for i = 0 to Cstruct.len file - 1 do
    Cstruct.set_uint8 file i 0
  done;
  Cstruct.blit_from_string contents 0 file 0 (String.length contents);
  return (`Ok file) *)

let searchClientTests =
    [
      "SearchClientWriteFile_EmptyString_ReadOutEmptyString", `Slow,
        (fun () ->
          check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newSearchClient () >>= fun searchClient ->
              newFile searchClient.SearchClient.fileSystem.F.blockDevice "")
            (fun () ->
              newSearchClient () >>= fun searchClient ->
              newFile searchClient.SearchClient.fileSystem.F.blockDevice "" >>= fun file ->
              SearchClient.writeFile searchClient "test" file >>= fun () ->
              SearchClient.readFile searchClient "test"));
      "SearchClientWriteFile_String_ReadOutString", `Slow,
        (fun () ->
          check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newSearchClient () >>= fun searchClient ->
              newFile searchClient.SearchClient.fileSystem.F.blockDevice "All work and no play makes Dave a dull boy")
            (fun () ->
              newSearchClient () >>= fun searchClient ->
              newFile searchClient.SearchClient.fileSystem.F.blockDevice "All work and no play makes Dave a dull boy" >>= fun file ->
              SearchClient.writeFile searchClient "test" file >>= fun () ->
              SearchClient.readFile searchClient "test"));
      "SearchClientWriteFile_ProjectGutenberg_ReadOutFileCorrectly", `Slow,
        (fun () ->
          let contents = readWholeFile "testFiles/pg61.txt" in
          check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newSearchClient () >>= fun searchClient ->
              newFile searchClient.SearchClient.fileSystem.F.blockDevice contents)
            (fun () ->
              newSearchClient () >>= fun searchClient ->
              newFile searchClient.SearchClient.fileSystem.F.blockDevice contents >>= fun file ->
              SearchClient.writeFile searchClient "test" file >>= fun () ->
              SearchClient.readFile searchClient "test"));
      "SearchClientSearch_3ProjectGutenbergsForGutenberg_AllFiles", `Slow,
        (fun () ->
          check (lwt_t @@ result error (list string)) ""
            (fun () -> return (`Ok ["pg63.txt";"pg61.txt";"pg62.txt"]))
            (fun () ->
              newSearchClient () >>= fun searchClient ->
              newFile searchClient.SearchClient.fileSystem.F.blockDevice (readWholeFile "testFiles/pg61.txt") >>= fun file1 ->
              newFile searchClient.SearchClient.fileSystem.F.blockDevice (readWholeFile "testFiles/pg62.txt") >>= fun file2 ->
              newFile searchClient.SearchClient.fileSystem.F.blockDevice (readWholeFile "testFiles/pg63.txt") >>= fun file3 ->
              SearchClient.writeFile searchClient "pg61.txt" file1 >>= fun () ->
              SearchClient.writeFile searchClient "pg62.txt" file2 >>= fun () ->
              SearchClient.writeFile searchClient "pg63.txt" file3 >>= fun () ->
              SearchClient.search searchClient "gutenberg"));
      (*)"ORAMFSWriteFile_3ProjectGutenbergs_ReadOutFile2Correctly", `Slow,
        (fun () ->
          check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/pg62.txt"))
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/pg61.txt") >>= fun file1 ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/pg62.txt") >>= fun file2 ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/pg63.txt") >>= fun file3 ->
              F.writeFile fs "pg61.txt" file1  >>= fun () ->
              F.writeFile fs "pg62.txt" file2  >>= fun () ->
              F.writeFile fs "pg63.txt" file3  >>= fun () ->
              F.readFile fs "pg62.txt"));
      "ORAMFSWriteFile_3ProjectGutenbergs_ReadOutFile3Correctly", `Slow,
        (fun () ->
          check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/pg63.txt"))
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/pg61.txt") >>= fun file1 ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/pg62.txt") >>= fun file2 ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/pg63.txt") >>= fun file3 ->
              F.writeFile fs "pg61.txt" file1  >>= fun () ->
              F.writeFile fs "pg62.txt" file2  >>= fun () ->
              F.writeFile fs "pg63.txt" file3  >>= fun () ->
              F.readFile fs "pg63.txt"));
      "ORAMFSWriteFile_DisconnectWhileWriting_ReadOutFile1Correctly", `Slow,
        (fun () ->
          check (lwt_t @@ result error cstruct) ""
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/pg61.txt"))
            (fun () ->
              newFs () >>= fun fs ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/pg61.txt") >>= fun file1 ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/pg62.txt") >>= fun file2 ->
              newFile fs.F.blockDevice (readWholeFile "testFiles/pg63.txt") >>= fun file3 ->
              F.writeFile fs "pg61.txt" file1  >>= fun () ->
              bind (O.disconnect fs.F.blockDevice) @@ fun () ->
              Block.connect "disk.img" >>= fun blockDevice ->
              O.fakeReconnect fs.F.blockDevice blockDevice >>= fun oram ->
              F.connect oram >>= fun fs ->
              F.writeFile fs "pg62.txt" file2  >>= fun () ->
              F.writeFile fs "pg63.txt" file3  >>= fun () ->
              F.readFile fs "pg61.txt"));*)
    ]

let () =
  Alcotest.run "Search Client Tests" [
    "Search Client Tests", searchClientTests
  ]
