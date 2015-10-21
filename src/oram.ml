open Lwt
open Printf
open V1_LWT
open Core_kernel.Std

module Make (B: BLOCK) = struct

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

  let initialise bd =
    lwt info = B.get_info bd in
    let dummy_struct = Cstruct.create info.sector_size in
    Cstruct.LE.set_uint64 dummy_struct 0 (-1L);
    (*Printf.printf "Initialising all block addresses to %d\n" (Option.value ~default:0 @@ Int64.to_int @@ Cstruct.LE.get_uint64 dummy_struct 0);*)
    for i = 8 to Cstruct.len dummy_struct - 1 do
      Cstruct.set_uint8 dummy_struct i 0
    done;
    let rec loop = function
      | 0L -> return (`Ok ())
      | x -> ignore_result (B.write bd Int64.(info.size_sectors - x) [dummy_struct]); loop Int64.(x - 1L)
    in
    match_lwt loop info.size_sectors with
      | `Ok () ->
        let buf = Cstruct.create info.sector_size in
        begin match_lwt B.read bd 1L [buf] with
          | `Ok () -> (*Printf.printf "All sectors now contain:\n%s\n" @@ String.escaped @@ Cstruct.to_string buf;*) return (`Ok ())
          | `Error x -> return (`Error x)
        end
      | `Error x -> return (`Error x)

  let connect bd =
    match_lwt initialise bd with
      | `Ok () ->
        lwt info = B.get_info bd in
        let read_write = info.read_write in
        if info.sector_size > 8
        then let sector_size = info.sector_size - 8 in
          let height = floor_log Int64.(info.size_sectors / 4L + 1L) - 1 in
          let size_sectors = Int64.(4L * (pow 2L (of_int height + 1L) - 1L)) in
          let stash = Stash.create () in
          let position = PosMap.create size_sectors in
          return (`Ok { info = { read_write; sector_size; size_sectors } ; height ; stash ; position ; bd })
        else return (`Error `Disconnected)
      | `Error x -> return (`Error x)

  let disconnect t =
    B.disconnect t.bd

  (*let encrypt x = *)

  let bucket_address x l =
    let rec loop acc = function
      | 0 -> acc
      | y ->
        if Int64.(bit_and (shift_right x l) 1L = 1L)
        then loop Int64.(2L * acc + 8L) (y - 1)
        else loop Int64.(2L * acc + 4L) (y - 1)
    in loop 0L l

    let write_bucket t a (b1,b2,b3,b4) =
      (*Printf.printf "Writing bucket at physical address %d...\n" (Option.value ~default:0 @@ Int64.to_int a);*)
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
            | [b] -> write_bucket t (bucket_address x t.height) b
            | _ -> return (`Error (`Unknown "Wrong number of buckets in path"))
          end
        | y ->
          match bs with
            | (b::bs) ->
              begin match_lwt write_bucket t (bucket_address x (t.height -y)) b with
                | `Ok () -> (*Printf.printf "Wrote bucket at level %d!\n" y;*) loop bs (y - 1)
                | `Error x -> return (`Error x)
              end
            | _ -> return (`Error (`Unknown "Wrong number of buckets in path"))
      in loop bs t.height

  let read_bucket t a =
    (*Printf.printf "Reading bucket at physical address %d...\n" (Option.value ~default:0 @@ Int64.to_int a);*)
    let sector_size = t.info.sector_size + 8 in
    let open Cstruct in
    let buf1 = create sector_size in
    let buf2 = create sector_size in
    let buf3 = create sector_size in
    let buf4 = create sector_size in
    (*Printf.printf "Cstructs successfully created, performing low level read...\n";*)
    match_lwt B.read t.bd a [buf1;buf2;buf3;buf4] with
      | `Ok () -> return (`Ok (OBlock.of_string @@ to_string buf1,
                               OBlock.of_string @@ to_string buf2,
                               OBlock.of_string @@ to_string buf3,
                               OBlock.of_string @@ to_string buf4))
      | `Error x -> return (`Error x)

  let read_path t x =
    let rec loop = function
      | 0 ->
        begin match_lwt read_bucket t (bucket_address x 0) with
          | `Ok b -> return (`Ok ([b]))
          | `Error x -> return (`Error x)
        end
      | y ->
        (*Printf.printf "Reading bucket at level %d...\n" y;*)
        begin match_lwt read_bucket t (bucket_address x y) with
            | `Ok b ->
              (*Printf.printf "Read bucket at level %d!\n" y;*)
              begin match_lwt loop (y - 1) with
                | `Ok bs -> return (`Ok (b::bs))
                | `Error x -> return (`Error x)
              end
            | `Error x -> return (`Error x)
        end
    in loop t.height

  let access t op a data' =
    (*Printf.printf "%s at block %d\n" (match op with | Read -> "Reading" | Write -> "Writing") (Option.value ~default:0 @@ Int64.to_int a);*)
    let x = PosMap.get t.position a in
    PosMap.set t.position a @@ Random.int64 Int64.(((t.info.size_sectors / 4L) + 1L) / 2L);
    (*Printf.printf "Remapped block %d to position %d\n" (Option.value ~default:0 @@ Int64.to_int a) (Option.value ~default:0 @@ Int64.to_int @@ PosMap.get t.position a);*)
    begin match_lwt read_path t x with
      | `Ok bs ->
        (*Printf.printf "Successfully read path to leaf %d\n" (Option.value ~default:0 @@ Int64.to_int x);*)
        List.iter bs ~f:(fun (b1, b2, b3, b4) ->
          (*match b1, b2, b3, b4 with
            | (a1,_), (a2,_), (a3,_), (a4,_) ->
              Printf.printf "Added blocks with the following addresses to the stash: %d %d %d %d\n"
                (Option.value ~default:0 @@ Int64.to_int a1)
                (Option.value ~default:0 @@ Int64.to_int a2)
                (Option.value ~default:0 @@ Int64.to_int a3)
                (Option.value ~default:0 @@ Int64.to_int a4);*)
          Stash.add t.stash b1;
          Stash.add t.stash b2;
          Stash.add t.stash b3;
          Stash.add t.stash b4);
        (*Printf.printf "Added values to stash\n";*)
        let data = match Stash.find_index t.stash a with
          | Some (_,d) -> d
          | None -> Bytes.create t.info.sector_size
        in
        (*Printf.printf "Found data in the stash for address %d as %s\n" (Option.value ~default:0 @@ Int64.to_int a) data;*)
        begin match op with
          | Write -> Hash_set.remove t.stash (a, data);
                     Stash.add t.stash (a, Option.value data' ~default:"")
          | Read -> ()
        end;
        let take_from_stash l =
          match Hash_set.find t.stash ~f:(fun (a',_) -> Int64.(bucket_address x l = bucket_address (PosMap.get t.position a') l)) with
            | Some b -> Hash_set.remove t.stash b;
                        b
            | None -> OBlock.dummy t.info.sector_size
        in
        let build_bucket l = (take_from_stash l, take_from_stash l, take_from_stash l, take_from_stash l) in
        let rec build_path acc = function
          | 0 -> (build_bucket 0) :: acc
          | x -> build_path ((build_bucket x) :: acc) (x - 1)
        in
        begin match_lwt write_path t x (build_path [] t.height) with
          | `Ok () -> (*Printf.printf "Returning data: %s\n" data;*) return (`Ok (data))
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
        (*Printf.printf "Writing part %d of buffer, which contains data: %s\n" x data;*)
        begin match_lwt access t Write Int64.(sector_start + (of_int x) - 1L) (Some data) with
          | `Ok (_) -> loop (x - 1)
          | `Error x -> return (`Error x)
        end
    in loop len

  let rec write t sector_start = function
    | [] -> return (`Ok ())
    | b :: bs ->
      match_lwt write_to_buffer t sector_start b with
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
            (*Printf.printf "Read data from buffer:\n%s\n" data;*)
            Cstruct.blit_from_string data 0 b (off + ((x - 1) * t.info.sector_size)) t.info.sector_size;
            loop (x - 1)
          | `Error x -> return (`Error x)
    in loop len

  let rec read t sector_start = function
    | [] -> return (`Ok ())
    | b :: bs ->
      match_lwt read_to_buffer t sector_start b with
        | `Ok () ->
          read t Int64.(sector_start + (of_int b.Cstruct.len / (of_int t.info.sector_size))) bs
        | `Error x ->
          return (`Error x)

end
