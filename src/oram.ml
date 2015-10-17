open Lwt
open Printf
open V1_LWT
open Core_kernel.Std

module Make (B: BLOCK) : BLOCK = struct

  type 'a io = 'a Lwt.t

  type page_aligned_buffer = Cstruct.t

  type error = [
    | `Unknown of string
    | `Unimplemented
    | `Is_read_only
    | `Disconnected
  ]

  type info = {
    read_write: bool;
    sector_size: int;
    size_sectors: int64;
  }

  type t = {
    info : info;
    oram : BucketTree.t;
    stash : Stash.t;
    position : PosMap.t;
    bd : B.t;
  }

  type op =
    | Read
    | Write

  let get_info { info } = return info

  let floor_log x =
    let open Int64 in
    let rec loop acc = function
      | 1L -> acc
      | x -> loop (acc + 1L) (x / 2L)
    in loop 0L x

  let connect bd =
    lwt info = B.get_info bd in
    let read_write = info.read_write in
    let sector_size = info.sector_size in
    let size_sectors = Int64.(pow 2L (floor_log @@ info.size_sectors + 1L) - 1L) in
    let oram = BucketTree.empty size_sectors in
    let stash = Stash.create () in
    let position = PosMap.create size_sectors in
    return (`Ok { info = { read_write; sector_size; size_sectors } ; oram ; stash ; position ; bd })

  let disconnect t =
    B.disconnect t.bd

  (*let encrypt x = *)

  let access t op a data' =
    let x = PosMap.get t.position a in
    PosMap.set t.position @@ Random.int64 Int64.(t.info.size_sectors + 1L / 2L);
    List.iter (BucketTree.read_path_to_leaf t.oram x) ~f:(fun (b1, b2, b3) ->
      Stash.add t.stash b1;
      Stash.add t.stash b2;
      Stash.add t.stash b3);
    let data = Stash.find_index t.stash (Some a) in
    begin match op with
      | Write -> Hash_set.remove t.stash (Some a, data);
                 Hash_set.add t.stash (Some a, Option.value data' ~default:"")
      | Read -> ()
    end;
    let take_from_stash l =
      match Hash_set.find t.stash ~f:(fun (a',_) ->
        match a' with
          | Some x' -> BucketTree.path_intercept x (PosMap.get t.position x') (BucketTree.height t.oram) l
          | None -> false)
      with
        | Some b -> Hash_set.remove t.stash b;
                    b
        | None -> OBlock.empty
    in
    let build_bucket l = (take_from_stash l, take_from_stash l, take_from_stash l) in
    let rec build_path acc = function
      | 0 -> (build_bucket 0) :: acc
      | x -> build_path ((build_bucket x) :: acc) (x - 1)
    in
    BucketTree.write_path_to_leaf_exn t.oram x (build_path [] (BucketTree.height t.oram));
    data

  let write_to_buffer t sector_start b =
    let off = b.Cstruct.off in
    let len = b.Cstruct.len / t.info.sector_size in
    let rec loop = function
      | 0 -> return (`Ok ())
      | x ->
        let data = Cstruct.copy b (off + ((x - 1) * t.info.sector_size)) t.info.sector_size in
        ignore (access t Write Int64.(sector_start + (of_int x) - 1L) (Some data));
        loop (x - 1)
    in loop len

  let write t sector_start = function
    | [] -> return (`Ok ())
    | b :: bs ->
      write_to_buffer t sector_start b >>= function
        | `Ok () ->
          write t Int64.(sector_start + (of_int b.Cstruct.len / (of_int t.info.sector_size))) bs
        | `Error x ->
          return (`Error x)

  let read_to_buffer t sector_start b =
    let off = b.Cstruct.off in
    let len = b.Cstruct.len / t.info.sector_size in
    let rec loop = function
      | 0 -> return (`Ok ())
      | x ->
        let data = access t Read Int64.(sector_start + (of_int x) - 1) None in
        Cstruct.blit_from_string data 0 b (off + ((x - 1) * t.info.sector_size)) t.info.sector_size;
        loop (x - 1)
    in loop len


  let rec read t sector_start = function
    | [] -> return (`Ok ())
    | b :: bs ->
      read_to_buffer t sector_start b >>= function
        | `Ok () ->
          read t Int64.(sector_start + (of_int b.Cstruct.len / (of_int t.info.sector_size))) bs
        | `Error x ->
          return (`Error x)

end
