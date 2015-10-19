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
    height : int;
    stash : Stash.t;
    position : PosMap.t;
    bd : B.t;
  }

  type id = string

  type op =
    | Read
    | Write

  let get_info { info } = return info

  let floor_log x =
    let rec loop acc = function
      | 1L -> acc
      | x -> loop (acc + 1) Int64.(x / 2L)
    in loop 0 x

  let connect bd =
    lwt info = B.get_info bd in
    let read_write = info.read_write in
    if info.sector_size > 8
    then let sector_size = info.sector_size - 8 in
      let height = floor_log Int64.(info.size_sectors + 1L) - 1 in
      let size_sectors = Int64.(pow 2L (of_int height + 1L) - 1L) in
      (*let oram = BucketTree.empty size_sectors in*)
      let stash = Stash.create () in
      let position = PosMap.create size_sectors in
      return (`Ok { info = { read_write; sector_size; size_sectors } ; height ; stash ; position ; bd })
    else return (`Error `Disconnected)

  let disconnect t =
    B.disconnect t.bd

  (*let encrypt x = *)

  let bucket_address x l =
    let rec loop acc = function
      | 0 -> acc
      | y ->
        if Int64.(bit_and (shift_right x l) 1L = 1L)
        then loop Int64.(2L * acc + 2L) (y - 1)
        else loop Int64.(2L * acc + 1L) (y - 1)
    in loop 0L l

    let write_bucket t a (b1,b2,b3,b4) =
      let open Cstruct in
      let buf1 = of_string @@ OBlock.to_string b1 in
      let buf2 = of_string @@ OBlock.to_string b2 in
      let buf3 = of_string @@ OBlock.to_string b3 in
      let buf4 = of_string @@ OBlock.to_string b4 in
      B.write t.bd a [buf1;buf2;buf3;buf4]

    let write_path t x bs =
      let rec loop bs = function
        | 0 ->
          begin match bs with
            | [b] -> write_bucket t (bucket_address x 0) b
            | _ -> return (`Error (`Unknown "Wrong number of buckets in path"))
          end
        | y ->
          match bs with
            | (b::bs) ->
              begin match_lwt write_bucket t (bucket_address x y) b with
                | `Ok () -> loop bs (y - 1)
                | `Error x -> return (`Error x)
              end
            | _ -> return (`Error (`Unknown "Wrong number of buckets in path"))
      in loop bs t.height

  let read_bucket t a =
    let sector_size = t.info.sector_size + 8 in
    let buffer = Cstruct.create (4 * sector_size) in
    let split4 x =
      let open Cstruct in
      let (c1,cs) = split x sector_size in
      let (c2,cs) = split cs sector_size in
      let (c3,cs) = split cs sector_size in
      let (c4,_) = split cs sector_size in
      (OBlock.of_string @@ to_string c1,
       OBlock.of_string @@ to_string c2,
       OBlock.of_string @@ to_string c3,
       OBlock.of_string @@ to_string c4)
    in
    B.read t.bd a [buffer] >>= function
      | `Ok () -> return (`Ok (split4 buffer))
      | `Error x -> return (`Error x)

  let read_path t x =
    let rec loop = function
      | 0 ->
        begin match_lwt read_bucket t (bucket_address x 0) with
          | `Ok b -> return (`Ok ([b]))
          | `Error x -> return (`Error x)
        end
      | y ->
        begin
          read_bucket t (bucket_address x y) >>= function
            | `Ok b ->
              begin
                loop (y - 1) >>= function
                  | `Ok bs -> return (`Ok (b::bs))
                  | `Error x -> return (`Error x)
              end
            | `Error x -> return (`Error x)
        end
    in loop t.height

  let access t op a data' =
    let x = PosMap.get t.position a in
    PosMap.set t.position x @@ Random.int64 Int64.(t.info.size_sectors + 1L / 2L);
    begin match_lwt read_path t x with
      | `Ok bs ->
        List.iter bs ~f:(fun (b1, b2, b3, b4) ->
          Stash.add t.stash b1;
          Stash.add t.stash b2;
          Stash.add t.stash b3;
          Stash.add t.stash b4);
        let data = Stash.find_index t.stash a in
        begin match op with
          | Write -> Hash_set.remove t.stash (a, data);
                     Stash.add t.stash (a, Option.value data' ~default:"")
          | Read -> ()
        end;
        let take_from_stash l =
          match Hash_set.find t.stash ~f:(fun (x',_) -> Int64.(bucket_address x l = bucket_address x' l)) with
            | Some b -> Hash_set.remove t.stash b;
                        b
            | None -> OBlock.dummy
        in
        let build_bucket l = (take_from_stash l, take_from_stash l, take_from_stash l, take_from_stash l) in
        let rec build_path acc = function
          | 0 -> (build_bucket 0) :: acc
          | x -> build_path ((build_bucket x) :: acc) (x - 1)
        in
        begin match_lwt write_path t x (build_path [] t.height) with
          | `Ok () -> return (`Ok (data))
          | `Error x -> return (`Error x)
        end
      | `Error x -> return (`Error x)
    end

  let write_to_buffer t sector_start b =
    let off = b.Cstruct.off in
    let len = b.Cstruct.len / t.info.sector_size in
    let rec loop = function
      | 0 -> return (`Ok ())
      | x ->
        let data = Cstruct.copy b (off + ((x - 1) * t.info.sector_size)) t.info.sector_size in
        begin match_lwt access t Write Int64.(sector_start + (of_int x) - 1L) (Some data) with
          | `Ok (_) -> loop (x - 1)
          | `Error x -> return (`Error x)
        end
    in loop len

  let rec write t sector_start = function
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
        match_lwt access t Read Int64.(sector_start + (of_int x) - 1L) None with
          | `Ok (data) ->
            Cstruct.blit_from_string data 0 b (off + ((x - 1) * t.info.sector_size)) t.info.sector_size;
            loop (x - 1)
          | `Error x -> return (`Error x)
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
