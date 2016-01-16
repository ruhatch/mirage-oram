open Alcotest
open Testable

let invertedIndexTests =
    [
      "InvertedIndexAdd_AddTest_FindTest", `Quick,
        (fun () ->
          let index = InvertedIndex.create () in
          let _ = InvertedIndex.updatePosting index "test" 1 in
          let result = InvertedIndex.find index "test" in
          check (list int) "" [1] result);
      "InvertedIndexToCstruct_AddTestToCstructBlockSize512_OfCstructFindTest", `Quick,
        (fun () ->
          let index = InvertedIndex.create () in
          let _ = InvertedIndex.updatePosting index "test" 1 in
          let cstruct = InvertedIndex.toCstruct index 512 in
          let newIndex = InvertedIndex.ofCstruct cstruct in
          let result = InvertedIndex.find newIndex "test" in
          check (list int) "" [1] result);
      "InvertedIndexToCstruct_AddTestToCstructBlockSize2048_OfCstructFindTest", `Quick,
        (fun () ->
          let index = InvertedIndex.create () in
          let _ = InvertedIndex.updatePosting index "test" 1 in
          let cstruct = InvertedIndex.toCstruct index 2048 in
          let newIndex = InvertedIndex.ofCstruct cstruct in
          let result = InvertedIndex.find newIndex "test" in
          check (list int) "" [1] result);
      "InvertedIndexIndexFile_SingleWord_ContainsWord", `Quick,
        (fun () ->
          let index = InvertedIndex.create () in
          InvertedIndex.indexFile index (Cstruct.of_string "word") 1;
          let result = InvertedIndex.find index "word" in
          check (list int) "" [1] result);
      "InvertedIndexIndexFile_TwoDocs_WordInBoth", `Quick,
        (fun () ->
          let index = InvertedIndex.create () in
          InvertedIndex.indexFile index (Cstruct.of_string "a long list of words") 1;
          InvertedIndex.indexFile index (Cstruct.of_string "a longer list of equally important words that might not be included") 2;
          let result = InvertedIndex.find index "words" in
          check (list int) "" [2;1] result);
    ]

let () = run "Inverted Index Tests" [
    "Inverted Index Tests", invertedIndexTests
  ]
