open Alcotest
open Lwt
open Testable

module IncreasedBlockSize = BlockSizeController.Make(Block)

module E = Block_ccm.Make(IncreasedBlockSize)

module O = Oram.Make(PosMap.InMemory)(E)

module F = Fs.Make(O)

let newFile (oram : O.t) contents =
  let%lwt info = O.get_info oram in
  let sectorLength = (String.length contents - 1) / info.O.sector_size + 1 in
  let file = Cstruct.create (sectorLength * info.O.sector_size) in
  for i = 0 to Cstruct.len file - 1 do
    Cstruct.set_uint8 file i 0
  done;
  Cstruct.blit_from_string contents 0 file 0 (String.length contents);
  return (`Ok file)

let newORAM ?(oramBlockSize = 0x2000) ?(desiredBlockSize = 0x4000) () =
  Block.connect "disk.img" >>= fun bd ->
  IncreasedBlockSize.connect ~desiredBlockSize bd >>= fun bd ->
  E.connect bd ~key:(Cstruct.of_string "keyofsixteenchar") >>= fun bd ->
  O.create ~desiredBlockSize:oramBlockSize bd

let newFs ?(oramBlockSize = 0x2000) ?(desiredBlockSize = 0x4000) () =
  newORAM ~oramBlockSize ~desiredBlockSize () >>= fun bd ->
  F.initialise bd

let encryptionTests1 =
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
            F.writeFile fs "test" file  >>= fun () ->
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
            F.writeFile fs "test" file  >>= fun () ->
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
            F.writeFile fs "pg61.txt" file1  >>= fun () ->
            F.writeFile fs "pg62.txt" file2  >>= fun () ->
            F.writeFile fs "pg63.txt" file3  >>= fun () ->
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
            F.writeFile fs "pg61.txt" file1  >>= fun () ->
            F.writeFile fs "pg62.txt" file2  >>= fun () ->
            F.writeFile fs "pg63.txt" file3  >>= fun () ->
            F.readFile fs "pg62.txt"));
    "ORAMFSWriteFile_3ProjectGutenbergs_ReadOutFile3Correctly", `Slow,
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
            F.writeFile fs "pg61.txt" file1  >>= fun () ->
            F.writeFile fs "pg62.txt" file2  >>= fun () ->
            F.writeFile fs "pg63.txt" file3  >>= fun () ->
            F.readFile fs "pg63.txt"));
    "ORAMFSWriteFile_DisconnectWhileWriting_ReadOutFile1Correctly", `Slow,
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
            F.writeFile fs "pg61.txt" file1  >>= fun () ->
            bind (O.disconnect fs.F.blockDevice) @@ fun () ->
            Block.connect "disk.img" >>= fun blockDevice ->
            IncreasedBlockSize.connect blockDevice >>= fun blockDevice ->
            E.connect blockDevice ~key:(Cstruct.of_string "keyofsixteenchar") >>= fun blockDevice ->
            O.fakeReconnect fs.F.blockDevice blockDevice >>= fun oram ->
            F.connect oram >>= fun fs ->
            F.writeFile fs "pg62.txt" file2  >>= fun () ->
            F.writeFile fs "pg63.txt" file3  >>= fun () ->
            F.readFile fs "pg61.txt"));
  ]

let encryptionTests ?(oramBlockSize = 0x40000) desiredBlockSize =
  [
    "ORAMFSWriteFile_EmptyString_ReadOutEmptyString", `Slow,
      (fun () ->
        check (lwt_t @@ result error cstruct) ""
          (fun () ->
            newFs ~oramBlockSize ~desiredBlockSize () >>= fun fs ->
            newFile fs.F.blockDevice "")
          (fun () ->
            newFs ~oramBlockSize ~desiredBlockSize () >>= fun fs ->
            newFile fs.F.blockDevice "" >>= fun file ->
            F.writeFile fs "test" file  >>= fun () ->
            F.readFile fs "test"));
    "ORAMFSWriteFile_String_ReadOutString", `Slow,
      (fun () ->
        check (lwt_t @@ result error cstruct) ""
          (fun () ->
            newFs ~oramBlockSize ~desiredBlockSize () >>= fun fs ->
            newFile fs.F.blockDevice "All work and no play makes Dave a dull boy")
          (fun () ->
            newFs ~oramBlockSize ~desiredBlockSize () >>= fun fs ->
            newFile fs.F.blockDevice "All work and no play makes Dave a dull boy" >>= fun file ->
            F.writeFile fs "test" file  >>= fun () ->
            F.readFile fs "test"));
    "ORAMFSWriteFile_ProjectGutenberg_ReadOutFileCorrectly", `Slow,
      (fun () ->
        let contents = readWholeFile "testFiles/gutenberg/pg61.txt" in
        check (lwt_t @@ result error cstruct) ""
          (fun () ->
            newFs ~oramBlockSize ~desiredBlockSize () >>= fun fs ->
            newFile fs.F.blockDevice contents)
          (fun () ->
            newFs ~oramBlockSize ~desiredBlockSize () >>= fun fs ->
            newFile fs.F.blockDevice contents >>= fun file ->
            F.writeFile fs "pg61.txt" file >>= fun () ->
            F.readFile fs "pg61.txt"));
    "ORAMFSWriteFile_3ProjectGutenbergs_ReadOutFile1Correctly", `Slow,
      (fun () ->
        check (lwt_t @@ result error cstruct) ""
          (fun () ->
            newFs ~oramBlockSize ~desiredBlockSize () >>= fun fs ->
            newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg61.txt"))
          (fun () ->
            newFs ~oramBlockSize ~desiredBlockSize () >>= fun fs ->
            newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg61.txt") >>= fun file1 ->
            newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg62.txt") >>= fun file2 ->
            newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg63.txt") >>= fun file3 ->
            F.writeFile fs "pg61.txt" file1  >>= fun () ->
            F.writeFile fs "pg62.txt" file2  >>= fun () ->
            F.writeFile fs "pg63.txt" file3  >>= fun () ->
            F.readFile fs "pg61.txt"));
    "ORAMFSWriteFile_3ProjectGutenbergs_ReadOutFile2Correctly", `Slow,
      (fun () ->
        check (lwt_t @@ result error cstruct) ""
          (fun () ->
            newFs ~oramBlockSize ~desiredBlockSize () >>= fun fs ->
            newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg62.txt"))
          (fun () ->
            newFs ~oramBlockSize ~desiredBlockSize () >>= fun fs ->
            newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg61.txt") >>= fun file1 ->
            newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg62.txt") >>= fun file2 ->
            newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg63.txt") >>= fun file3 ->
            F.writeFile fs "pg61.txt" file1  >>= fun () ->
            F.writeFile fs "pg62.txt" file2  >>= fun () ->
            F.writeFile fs "pg63.txt" file3  >>= fun () ->
            F.readFile fs "pg62.txt"));
    "ORAMFSWriteFile_3ProjectGutenbergs_ReadOutFile3Correctly", `Slow,
      (fun () ->
        check (lwt_t @@ result error cstruct) ""
          (fun () ->
            newFs ~oramBlockSize ~desiredBlockSize () >>= fun fs ->
            newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg63.txt"))
          (fun () ->
            newFs ~oramBlockSize ~desiredBlockSize () >>= fun fs ->
            newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg61.txt") >>= fun file1 ->
            newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg62.txt") >>= fun file2 ->
            newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg63.txt") >>= fun file3 ->
            F.writeFile fs "pg61.txt" file1  >>= fun () ->
            F.writeFile fs "pg62.txt" file2  >>= fun () ->
            F.writeFile fs "pg63.txt" file3  >>= fun () ->
            F.readFile fs "pg63.txt"));
    "ORAMFSWriteFile_DisconnectWhileWriting_ReadOutFile1Correctly", `Slow,
      (fun () ->
        check (lwt_t @@ result error cstruct) ""
          (fun () ->
            newFs ~oramBlockSize ~desiredBlockSize () >>= fun fs ->
            newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg61.txt"))
          (fun () ->
            newFs ~oramBlockSize ~desiredBlockSize () >>= fun fs ->
            newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg61.txt") >>= fun file1 ->
            newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg62.txt") >>= fun file2 ->
            newFile fs.F.blockDevice (readWholeFile "testFiles/gutenberg/pg63.txt") >>= fun file3 ->
            F.writeFile fs "pg61.txt" file1  >>= fun () ->
            bind (O.disconnect fs.F.blockDevice) @@ fun () ->
            Block.connect "disk.img" >>= fun blockDevice ->
            IncreasedBlockSize.connect ~desiredBlockSize blockDevice >>= fun blockDevice ->
            E.connect blockDevice ~key:(Cstruct.of_string "keyofsixteenchar") >>= fun blockDevice ->
            O.fakeReconnect fs.F.blockDevice blockDevice >>= fun oram ->
            F.connect oram >>= fun fs ->
            F.writeFile fs "pg62.txt" file2  >>= fun () ->
            F.writeFile fs "pg63.txt" file3  >>= fun () ->
            F.readFile fs "pg61.txt"));
  ]

let blockSizes = [0x200;0x400;0x800;0x1000;0x2000;0x4000;0x8000;0x10000;0x20000;0x40000;0x80000;0x100000]

let blockSizePairs = List.flatten @@ List.map (fun blockSize1 -> List.map (fun blockSize2 -> (blockSize1, blockSize2)) blockSizes) blockSizes

let normalTests = [ "Encryption Tests", encryptionTests1 ]

let underlyingBlockSizeTests = List.mapi (fun index blockSize -> (Printf.sprintf "Encryption Tests %d" index, encryptionTests blockSize)) blockSizes

let bothBlockSizeTests = List.mapi (fun index (blockSize1, blockSize2) -> (Printf.sprintf "Encryption Tests %d" index, encryptionTests ~oramBlockSize:blockSize1 blockSize2)) blockSizePairs

let () =
  ignore @@ Nocrypto_entropy_lwt.initialize ();
  Alcotest.run "Encryption Tests" normalTests
