open Alcotest
open Testable

let invertedIndexTests =
    [
      "InvertedIndexAdd_AddTest_FindTest", `Quick,
        (fun () ->
          let index = InvertedIndex.create () in
          let _ = InvertedIndex.updatePosting index "test" "test.ml" in
          let result = InvertedIndex.fileNamesForQuery index "test" in
          check (list string) "" ["test.ml"] result);
      "InvertedIndexToCstruct_AddTestToCstructBlockSize512_OfCstructFindTest", `Quick,
        (fun () ->
          let index = InvertedIndex.create () in
          let _ = InvertedIndex.updatePosting index "test" "test.ml" in
          let cstruct = InvertedIndex.toCstruct index 512 in
          let newIndex = InvertedIndex.ofCstruct cstruct in
          let result = InvertedIndex.fileNamesForQuery newIndex "test" in
          check (list string) "" ["test.ml"] result);
      "InvertedIndexToCstruct_AddTestToCstructBlockSize2048_OfCstructFindTest", `Quick,
        (fun () ->
          let index = InvertedIndex.create () in
          let _ = InvertedIndex.updatePosting index "test" "test.ml" in
          let cstruct = InvertedIndex.toCstruct index 2048 in
          let newIndex = InvertedIndex.ofCstruct cstruct in
          let result = InvertedIndex.fileNamesForQuery newIndex "test" in
          check (list string) "" ["test.ml"] result);
      "InvertedIndexIndexFile_SingleWord_ContainsWord", `Quick,
        (fun () ->
          let index = InvertedIndex.create () in
          InvertedIndex.indexFile index "test.ml" (Cstruct.of_string "word");
          let result = InvertedIndex.fileNamesForQuery index "word" in
          check (list string) "" ["test.ml"] result);
      "InvertedIndexIndexFile_TwoDocs_WordInBoth", `Quick,
        (fun () ->
          let index = InvertedIndex.create () in
          InvertedIndex.indexFile index "test.ml" (Cstruct.of_string "a long list of words");
          InvertedIndex.indexFile index "test2.ml" (Cstruct.of_string "a longer list of equally important words that might not be included");
          let result = InvertedIndex.fileNamesForQuery index "words" in
          check (list string) "" ["test2.ml" ; "test.ml"] result);
      "InvertedIndexIndexFile_FourDocs_PhraseInTwo", `Quick,
        (fun () ->
          let index = InvertedIndex.create () in
          InvertedIndex.indexFile index "test.ml" (Cstruct.of_string "a long list of words");
          InvertedIndex.indexFile index "test2.ml" (Cstruct.of_string "a longer list of equally important words that might not be included");
          InvertedIndex.indexFile index "test3.ml" (Cstruct.of_string "some long words");
          InvertedIndex.indexFile index "test4.ml" (Cstruct.of_string "a longer list of equally important terms");
          let result = InvertedIndex.fileNamesForQuery index "words list" in
          check (list string) "" ["test2.ml" ; "test.ml"] result);
    ]

let () = run "Inverted Index Tests" [
    "Inverted Index Tests", invertedIndexTests
  ]
