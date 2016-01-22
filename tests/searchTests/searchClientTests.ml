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
    ]

let () =
  Alcotest.run "Search Client Tests" [
    "Search Client Tests", searchClientTests
  ]
