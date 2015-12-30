open Alcotest
open Testable
open Lwt

module P = PosMap.InMemory(Block)
(* module P = Oram.Make(PosMap.InMemory)(Block) *)


let ( >>= ) x f = x >>= function
  | `Error e -> return (`Error e)
  | `Ok x -> f x

let newPosMap size =
  Block.connect "disk.img" >>= fun bd ->
  (*P.initialise bd >>= fun () ->*)
  P.create ~size ~offset:1L bd

let posmap_tests = [
    (*)"PosMapIndices_Zero_ZeroZero", `Quick,
      (fun () ->
        Alcotest.(check @@ pair int int) "PosMapIndices_Zero_ZeroZero" (0,0) (PosMap.indices 0L));
    "PosMapIndices_MaxInt_Max32Max32", `Quick,
      (fun () ->
        Alcotest.(check @@ pair int int) "" (2147483647,4294967295) (PosMap.indices Int64.max_int));
    "PosMapIndices_Eighty_ZeroEighty", `Quick,
      (fun () ->
        Alcotest.(check @@ pair int int) "" (0,80) (PosMap.indices 80L));*)
    (*)"PosMapSet_OutOfBounds_Error", `Quick,
      (fun () ->
        newPosMap () >>= fun posmap ->
        Alcotest.check_raises "Out of Bounds" (Invalid_argument "index out of bounds") (fun () -> P.set posmap 100L 1L));*)
    "PosMapSet_InBounds_ValueSet", `Quick,
      (fun () ->
        (check (lwt_t @@ result error int64)) ""
          (fun () -> return (`Ok 100L))
          (fun () ->
            newPosMap 10000L >>= fun posmap ->
            P.set posmap 1L 100L >>= fun () ->
            P.get posmap 1L));
    (*)"PosMapGet_OutOfBounds_Error", `Quick,
      (fun () ->
        let posmap = P.create 100L in
        Alcotest.check_raises "Out of Bounds" (Invalid_argument "index out of bounds") (fun () -> ignore @@ P.get posmap 100L));*)
    (*)"PosMapLength_LessThanMax32_SameSize", `Quick,
      (fun () ->
        (check (lwt_t @@ result error int64)) ""
          (fun () -> return (`Ok 100L))
          (fun () ->
            newPosMap 100L >>= fun posmap ->
            return (`Ok (P.length posmap))));*)
  ]

let () =
  Alcotest.run "PosMap Tests" [
    "PosMap Tests", posmap_tests
  ]
