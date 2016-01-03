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
    "PosMapIndices_Zero_ZeroZero", `Quick,
      (fun () ->
        (check @@ tuple3 int int int) "PosMapIndices_Zero_ZeroZeroZero" (0, 0, 0) (P.indices 0L));
    "PosMapIndices_MaxInt_7Max30Max30", `Quick,
      (fun () ->
        (check @@ tuple3 int int int) "" (7, 0x3FFFFFFF, 0x3FFFFFFF) (P.indices Int64.max_int));
    "PosMapIndices_Eighty_ZeroZeroEighty", `Quick,
      (fun () ->
        (check @@ tuple3 int int int) "" (0, 0, 80) (P.indices 80L));
    "PosMapSet_OutOfBounds_Error", `Quick,
      (fun () ->
        check_raises "Out of Bounds" (Invalid_argument "index out of bounds") (fun () -> ignore (Lwt_main.run (newPosMap 100L >>= fun posmap -> P.set posmap 100L 1L))));
    "PosMapSet_InBounds_ValueSet", `Quick,
      (fun () ->
        (check (lwt_t @@ result error int64)) ""
          (fun () -> return (`Ok 100L))
          (fun () ->
            newPosMap 10000L >>= fun posmap ->
            P.set posmap 1L 100L >>= fun () ->
            P.get posmap 1L));
    "PosMapLength_LessThanMax32_4TimesClosestPowerOf2Minus1", `Quick,
      (fun () ->
        (check (lwt_t @@ result error int64)) ""
          (fun () -> return (`Ok 60L))
          (fun () ->
            newPosMap 100L >>= fun posmap ->
            return (`Ok (P.length posmap))));
  ]

let () =
  Alcotest.run "PosMap Tests" [
    "PosMap Tests", posmap_tests
  ]
