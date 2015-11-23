open Alcotest
open Testable
open InodeIndex
open Lwt

module I = Make(Block)

let bd =
  match_lwt Block.connect "disk.img" with
    | `Ok bd -> return bd
    | `Error x -> failwith "Failed to connect to raw Block"

let info = bd >>= fun bd -> Block.get_info bd

let freeMap = info >>= fun info -> return (FreeMap.create info.Block.sector_size)

let newIndex () =
  freeMap >>= fun freeMap ->
  bd >>= fun bd ->
  info >>= fun info ->
  I.create freeMap bd info.Block.sector_size

let ( >>= ) x f = x >>= function
  | `Error e -> return (`Error e)
  | `Ok x -> f x

(* Some of these tests are kind of random and should really be located in other places *)

let inode_index_tests =
    [
      "InodeIndexCreate_NewIndex_RootAddressThree", `Quick,
        (fun () ->
          check (lwt_t @@ result error int64) ""
            (fun () -> return (`Ok 3L))
            (fun () -> newIndex () >>= fun inodeIndex -> return (`Ok inodeIndex.I.rootAddress)));
      "InodeIndexCreate_NewIndex_RootContainsPageSize", `Quick,
        (fun () ->
          check (lwt_t @@ result error int) ""
            (fun () -> bind info (fun info -> return (`Ok info.Block.sector_size)))
            (fun () -> newIndex () >>= fun inodeIndex -> return (`Ok (Node.Node.pageSize inodeIndex.I.root))));
      "InodeIndexCreate_NewIndex_RootNoKeysZero", `Quick,
        (fun () ->
          check (lwt_t @@ result error int) ""
            (fun () -> return (`Ok 0))
            (fun () -> newIndex () >>= fun inodeIndex -> return (`Ok (Node.Node.noKeys inodeIndex.I.root))));
      "InodeIndexCreate_NewIndex_MinDegree14", `Quick,
        (fun () ->
          check (lwt_t @@ result error int) ""
            (fun () -> return (`Ok 14))
            (fun () -> newIndex () >>= fun inodeIndex -> return (`Ok (inodeIndex.I.minDegree))));
      "InodeIndexInsert_KeyOneValue100_KeyOne", `Quick,
        (fun () ->
          check (lwt_t @@ result error int) ""
            (fun () -> return (`Ok 1))
            (fun () -> newIndex () >>= fun inodeIndex ->
              I.insert inodeIndex 1 100L >>= fun () ->
              return (`Ok (Node.Node.getKey inodeIndex.I.root 1))));
      "InodeIndexInsert_KeyOneValue100_Value100", `Quick,
        (fun () ->
          check (lwt_t @@ result error int64) ""
            (fun () -> return (`Ok 100L))
            (fun () -> newIndex () >>= fun inodeIndex ->
              I.insert inodeIndex 1 100L >>= fun () ->
              return (`Ok (Node.Node.getValue inodeIndex.I.root 1))));
      "InodeIndexInsert_Key5Key1Key3_ValueOfKey3AtPosition2", `Quick,
        (fun () ->
          check (lwt_t @@ result error int64) ""
            (fun () -> return (`Ok 3L))
            (fun () -> newIndex () >>= fun inodeIndex ->
              I.insert inodeIndex 5 5L >>= fun () ->
              I.insert inodeIndex 1 1L >>= fun () ->
              I.insert inodeIndex 3 3L >>= fun () ->
              return (`Ok (Node.Node.getValue inodeIndex.I.root 2))));
      "InodeIndexInsert_28Keys_NewRootMedianValueInPosition1", `Quick,
        (fun () ->
          check (lwt_t @@ result error int) ""
            (fun () -> return (`Ok 15))
            (fun () -> newIndex () >>= fun inodeIndex ->
              let rec loop = function
                | 0 -> return (`Ok ())
                | n ->
                  I.insert inodeIndex n (Int64.of_int n) >>= fun () ->
                  loop (n - 1)
              in loop 28 >>= fun () ->
              return (`Ok (Node.Node.getKey inodeIndex.I.root 1))));
      "InodeIndexInsert_27Keys_14AtMedianPosition", `Quick,
        (fun () ->
          check (lwt_t @@ result error int) ""
            (fun () -> return (`Ok 14))
            (fun () -> newIndex () >>= fun inodeIndex ->
              let rec loop = function
                | 0 -> return (`Ok ())
                | n ->
                  I.insert inodeIndex n (Int64.of_int n) >>= fun () ->
                  loop (n - 1)
              in loop 27 >>= fun () ->
              return (`Ok (Node.Node.getKey inodeIndex.I.root 14))));
      "InodeIndexInsert_27Keys_DontFind28", `Quick,
        (fun () ->
          check (lwt_t @@ result error int64) ""
            (fun () -> return (`Ok 0L))
            (fun () -> newIndex () >>= fun inodeIndex ->
              let rec loop = function
                | 0 -> return (`Ok ())
                | n ->
                  I.insert inodeIndex n (Int64.of_int n) >>= fun () ->
                  loop (n - 1)
              in loop 27 >>= fun () ->
              I.find inodeIndex inodeIndex.I.root 28 >>= function
                | Some a -> return (`Ok a)
                | None -> return (`Ok 0L)));
      "InodeIndexInsert_28Keys_FindValue5AtKey5", `Quick,
        (fun () ->
          check (lwt_t @@ result error int64) ""
            (fun () -> return (`Ok 5L))
            (fun () -> newIndex () >>= fun inodeIndex ->
              let rec loop = function
                | 0 -> return (`Ok ())
                | n ->
                  I.insert inodeIndex n (Int64.of_int n) >>= fun () ->
                  loop (n - 1)
              in loop 28 >>= fun () ->
              I.find inodeIndex inodeIndex.I.root 5 >>= function
                | Some a -> return (`Ok a)
                | None -> return (`Ok 0L)));
      "InodeIndexInsert_28Keys_FindValue28AtKey28", `Quick,
        (fun () ->
          check (lwt_t @@ result error int64) ""
            (fun () -> return (`Ok 28L))
            (fun () -> newIndex () >>= fun inodeIndex ->
              let rec loop = function
                | 0 -> return (`Ok ())
                | n ->
                  I.insert inodeIndex n (Int64.of_int n) >>= fun () ->
                  loop (n - 1)
              in loop 28 >>= fun () ->
              I.find inodeIndex inodeIndex.I.root 28 >>= function
                | Some a -> return (`Ok a)
                | None -> return (`Ok 0L)));
      "InodeIndexInsert_28Keys_FindValue14AtKey14", `Quick,
        (fun () ->
          check (lwt_t @@ result error int64) ""
            (fun () -> return (`Ok 14L))
            (fun () -> newIndex () >>= fun inodeIndex ->
              let rec loop = function
                | 0 -> return (`Ok ())
                | n ->
                  I.insert inodeIndex n (Int64.of_int n) >>= fun () ->
                  loop (n - 1)
              in loop 28 >>= fun () ->
              I.find inodeIndex inodeIndex.I.root 14 >>= function
                | Some a -> return (`Ok a)
                | None -> return (`Ok 0L)));
    ]

let () =
  Alcotest.run "Inode Index Tests" [
    "Inode Index Tests", inode_index_tests
  ]
