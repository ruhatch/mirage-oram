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

(* Some of these tests are kind of random and should really be located in other places *)

let inode_index_tests =
    [
      "InodeIndexCreate_NewIndex_RootAddressThree", `Quick,
        (fun () ->
          check (lwt_t int64) ""
            (fun () -> return 3L)
            (fun () -> newIndex () >>= fun inodeIndex -> return inodeIndex.I.rootAddress));
      "InodeIndexCreate_NewIndex_RootContainsPageSize", `Quick,
        (fun () ->
          check (lwt_t int) ""
            (fun () -> info >>= fun info -> return (info.Block.sector_size))
            (fun () -> newIndex () >>= fun inodeIndex -> return (Node.Node.pageSize inodeIndex.I.root)));
      "InodeIndexCreate_NewIndex_RootNoKeysZero", `Quick,
        (fun () ->
          check (lwt_t int) ""
            (fun () -> return 0)
            (fun () -> newIndex () >>= fun inodeIndex -> return (Node.Node.noKeys inodeIndex.I.root)));
      "InodeIndexCreate_NewIndex_MinDegree14", `Quick,
        (fun () ->
          check (lwt_t int) ""
            (fun () -> return 14)
            (fun () -> newIndex () >>= fun inodeIndex -> return (inodeIndex.I.minDegree)));
      "InodeIndexInsert_KeyOneValue100_KeyOne", `Quick,
        (fun () ->
          check (lwt_t int) ""
            (fun () -> return 1)
            (fun () -> newIndex () >>= fun inodeIndex ->
              match_lwt I.insert inodeIndex 1 100L with
                | `Ok () -> return (Node.Node.getKey inodeIndex.I.root 1)
                | `Error x -> failwith "Insertion failure"));
      "InodeIndexInsert_KeyOneValue100_Value100", `Quick,
        (fun () ->
          check (lwt_t int64) ""
            (fun () -> return 100L)
            (fun () -> newIndex () >>= fun inodeIndex ->
              match_lwt I.insert inodeIndex 1 100L with
                | `Ok () -> return (Node.Node.getValue inodeIndex.I.root 1)
                | `Error x -> failwith "Insertion failure"));
      "InodeIndexInsert_Key5Key1Key3_ValueOfKey3AtPosition2", `Quick,
        (fun () ->
          check (lwt_t int64) ""
            (fun () -> return 3L)
            (fun () -> newIndex () >>= fun inodeIndex ->
              match_lwt I.insert inodeIndex 5 5L with
                | `Ok () -> (
                  match_lwt I.insert inodeIndex 1 1L with
                    | `Ok () -> (
                      match_lwt I.insert inodeIndex 3 3L with
                        | `Ok () -> return (Node.Node.getValue inodeIndex.I.root 2)
                        | `Error x -> failwith "Insertion failure"
                    )
                    | `Error x -> failwith "Insertion failure"
                )
                | `Error x -> failwith "Insertion failure"));
      "InodeIndexInsert_28Keys_NewRootMedianValueInPosition1", `Quick,
        (fun () ->
          check (lwt_t int) ""
            (fun () -> return 15)
            (fun () -> newIndex () >>= fun inodeIndex ->
              let rec loop = function
                | 0 -> return ()
                | n ->
                  match_lwt I.insert inodeIndex n (Int64.of_int n) with
                    | `Ok () -> loop (n - 1)
                    | `Error x -> failwith "Insertion failure"
              in loop 28 >>= fun () ->
              return (Node.Node.getKey inodeIndex.I.root 1)));
      "InodeIndexInsert_27Keys_14AtMedianPosition", `Quick,
        (fun () ->
          check (lwt_t int) ""
            (fun () -> return 14)
            (fun () -> newIndex () >>= fun inodeIndex ->
              let rec loop = function
                | 0 -> return ()
                | n ->
                  match_lwt I.insert inodeIndex n (Int64.of_int n) with
                    | `Ok () -> loop (n - 1)
                    | `Error x -> failwith "Insertion failure"
              in loop 27 >>= fun () ->
              return (Node.Node.getKey inodeIndex.I.root 14)));
      "InodeIndexInsert_27Keys_DontFind28", `Quick,
        (fun () ->
          check (lwt_t int) ""
            (fun () -> return 0)
            (fun () -> newIndex () >>= fun inodeIndex ->
              let rec loop = function
                | 0 -> return ()
                | n ->
                  match_lwt I.insert inodeIndex n (Int64.of_int n) with
                    | `Ok () -> loop (n - 1)
                    | `Error x -> failwith "Insertion failure"
              in loop 27 >>= fun () ->
              match_lwt I.find inodeIndex inodeIndex.I.root 28 with
                | Some (node,i) -> return (Node.Node.getKey node i)
                | None -> return 0));
      "InodeIndexInsert_28Keys_5AtPos5", `Quick,
        (fun () ->
          check (lwt_t int) ""
            (fun () -> return 5)
            (fun () -> newIndex () >>= fun inodeIndex ->
              let rec loop = function
                | 0 -> return ()
                | n ->
                  match_lwt I.insert inodeIndex n (Int64.of_int n) with
                    | `Ok () -> loop (n - 1)
                    | `Error x -> failwith "Insertion failure"
              in loop 28 >>= fun () ->
              match_lwt I.find inodeIndex inodeIndex.I.root 5 with
                | Some (node,i) -> return (Node.Node.getKey node i)
                | None -> return 0));
      (*)"InodeIndexInsert_28Keys_19AtPos5", `Quick,
        (fun () ->
          check (lwt_t int) ""
            (fun () -> return 5)
            (fun () -> newIndex () >>= fun inodeIndex ->
              let rec loop = function
                | 0 -> return ()
                | n ->
                  match_lwt I.insert inodeIndex n (Int64.of_int n) with
                    | `Ok () -> loop (n - 1)
                    | `Error x -> failwith "Insertion failure"
              in loop 28 >>= fun () ->
              match_lwt I.find inodeIndex inodeIndex.I.root 19 with
                | Some (node,i) -> return i
                | None -> return 0));*)
    ]

let () =
  Alcotest.run "Inode Index Tests" [
    "Inode Index Tests", inode_index_tests
  ]
