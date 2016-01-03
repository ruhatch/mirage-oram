open Lwt
open Printf
open V1_LWT
open Core_kernel.Std
open PosMapIntf

module Make (PF: PosMapF)(B: BLOCK) = struct

  module P = PF(B)

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
    bucketSize : int64;
    stash : Stash.t;
    posMap : P.t;
    bd : B.t;
    offset : int64;
    (*output : Out_channel.t;*)
  }

  type id = string

  type op =
    | Read
    | Write

  type bucket = OBlock.t list

  let ( >>= ) x f = x >>= function
    | `Error e -> return (`Error e)
    | `Ok x -> f x

  let get_info { info } = return info

  let printInts cstruct =
    Printf.printf "Cstruct:";
    for i = 0 to (Cstruct.len cstruct / 8) - 1 do
      Printf.printf " %Ld" (Cstruct.BE.get_uint64 cstruct (i * 8))
    done;
    Printf.printf "\n"

  let floor_log x =
    let rec loop acc = function
      | 1L -> acc
      | x -> loop (acc + 1) Int64.(x / 2L)
    in loop 0 x

  let initialise bd =
    lwt info = B.get_info bd in
    let dummy_struct = Cstruct.create (info.B.sector_size * 512) in
    Cstruct.LE.set_uint64 dummy_struct 0 (-1L);
    (*Printf.printf "Initialising all block addresses to %d\n" (Option.value ~default:0 @@ Int64.to_int @@ Cstruct.LE.get_uint64 dummy_struct 0);*)
    for i = 8 to Cstruct.len dummy_struct - 1 do
      Cstruct.set_uint8 dummy_struct i 0
    done;
    let rec loop = function
      | 0L -> return (`Ok ())
      | x -> B.write bd Int64.((x - 1L) * 512L) [dummy_struct] >>= fun () -> loop Int64.(x - 1L)
    in
    loop Int64.(info.B.size_sectors / 512L)

  (*let newLog () =
    let time = Unix.gmtime (Unix.time ()) in
    let twoDigit i = if i > 9 then Printf.sprintf "%d" i else Printf.sprintf "0%d" i in
    let filename = Printf.sprintf "oramOutput%d%d%d_%s%s%s" time.Unix.tm_mday time.Unix.tm_mon time.Unix.tm_year (twoDigit time.Unix.tm_hour) (twoDigit time.Unix.tm_min) (twoDigit time.Unix.tm_sec) in
    let output = Out_channel.create filename in
    output*)

  (*let connect bd =
    initialise bd >>= fun () ->
      lwt info = B.get_info bd in
      let read_write = info.B.read_write in
      if info.B.sector_size > 8
      then let sector_size = info.B.sector_size - 8 in
        let height = floor_log Int64.(info.B.size_sectors / 4L + 1L) - 1 in
        let size_sectors = Int64.(4L * (pow 2L (of_int height + 1L) - 1L)) in
        let stash = Stash.create () in
        P.create ~size:size_sectors bd >>= fun posMap ->
        (*let output = newLog () in*)
        (*Random.self_init ();*)
        return (`Ok { info = { read_write; sector_size; size_sectors } ; height ; stash ; posMap ; bd ; offset = 0L (*); output*) })
      else return (`Error `Disconnected)*)

  let disconnect t =
    B.disconnect t.bd

  (*let encrypt x = *)

  let bucket_address t x l =
    let rec loop acc = function
      | 0 -> acc
      | y ->
        if Int64.bit_and (Int64.shift_right x (y + (t.height - l) - 1)) 1L = 1L
          (* temporarily added multiplication of sixteen to offsets *)
          then loop Int64.(2L * acc + (t.bucketSize * 1024L)) (y - 1)
          else loop Int64.(2L * acc + (t.bucketSize * 512L)) (y - 1)
    in loop 0L l

  let write_bucket t a bs =
    let buffers = List.map ~f:OBlock.to_cstruct bs in
    (*if t.offset = 0L
      then Printf.printf "Writing bucket at physical address %Ld\n" Int64.(a + t.offset);*)
    (*let buf1 = OBlock.to_cstruct b1 in
    let buf2 = OBlock.to_cstruct b2 in
    let buf3 = OBlock.to_cstruct b3 in
    let buf4 = OBlock.to_cstruct b4 in*)
    B.write t.bd Int64.(a + t.offset) buffers (*[ buf1 ; buf2 ; buf3 ; buf4 ]*)

  let write_path t x bs =
    let rec loop bs = function
      | 0 ->
        begin match bs with
          | [b] -> write_bucket t (bucket_address t x t.height) b
          | _ -> return (`Error (`Unknown "Wrong number of buckets in path"))
        end
      | y ->
        match bs with
          | (b::bs) ->
            write_bucket t (bucket_address t x (t.height - y)) b >>= fun () ->
            loop bs (y - 1)
          | _ -> return (`Error (`Unknown "Wrong number of buckets in path"))
    in loop bs t.height

  let read_bucket t a =
    (*if t.offset = 0L then Printf.printf "Reading bucket at physical address %Ld\n" Int64.(a + t.offset);*)
    let sector_size = t.info.sector_size + 8 in
    let buffers = List.map ~f:(fun _ -> Cstruct.create sector_size) (List.range 0 (Option.value (Int64.to_int t.bucketSize) ~default:4)) in
    (*let buf1 = Cstruct.create sector_size in
    let buf2 = Cstruct.create sector_size in
    let buf3 = Cstruct.create sector_size in
    let buf4 = Cstruct.create sector_size in*)
    B.read t.bd Int64.(a + t.offset) buffers (*[ buf1 ; buf2 ; buf3 ; buf4 ]*) >>= fun () ->
    return (`Ok (List.map ~f:OBlock.of_cstruct buffers)(*(OBlock.of_cstruct buf1, OBlock.of_cstruct buf2, OBlock.of_cstruct buf3, OBlock.of_cstruct buf4)*))

  let read_path t x =
    let rec loop = function
      | 0 ->
        read_bucket t (bucket_address t x 0) >>= fun b ->
        return (`Ok ([b]))
      | y ->
        read_bucket t (bucket_address t x y) >>= fun b ->
        loop (y - 1) >>= fun bs ->
        return (`Ok (b::bs))
    in loop t.height

  (* New path building function

    Input: List of addresses in the stash and their positions
    Output: Full path *)

  let validBlocks t l x = List.partition_tf ~f:(fun (pos, _) -> Int64.shift_right pos (t.height - l) = Int64.shift_right x (t.height - l))

  let buildBucket t choices =
    let rec loop left acc rest =
      match left, acc, rest with
      | 0L, bs, rest -> (bs, rest)
      | n, acc, [] -> loop Int64.(n - 1L) (OBlock.dummy t.info.sector_size :: acc) []
      | n, acc, ((_, (a,d)) :: xs) -> Stash.remove t.stash a; loop Int64.(n - 1L) ((a,d) :: acc) xs
    in loop t.bucketSize [] choices

  let rec buildPath t acc stash x = function
    | 0 ->
      let (hits, misses) = validBlocks t 0 x stash in
      let (bucket, rest) = buildBucket t hits in
      bucket :: acc
    | l ->
      let (hits, misses) = validBlocks t l x stash in
      let (bucket, rest) = buildBucket t hits in
      buildPath t (bucket :: acc) (rest @ misses) x (l - 1)

  let access t op a data' =
    P.get t.posMap a >>= fun x ->
    (*if t.offset > 0L then Printf.printf "%s Oram at offset %Ld at block %Ld with leaf %Ld\n" (match op with | Read -> "Reading" | Write -> "Writing") t.offset a x;*)
    (*Out_channel.output_string t.output @@ Printf.sprintf "Oram at offset %Ld accessing path to leaf %Ld" t.offset x;
    Out_channel.newline t.output;
    Out_channel.flush t.output;*)
    let newLeaf = Random.int64 Int64.(((t.info.size_sectors / t.bucketSize) + 1L) / 2L) in
    P.set t.posMap a newLeaf >>= fun () ->
    (*P.get t.posMap a >>= fun newLeaf' ->
    if t.offset = 0L then Printf.printf "Attempted to remap block %Ld to position %Ld and it is at %Ld\n" a newLeaf newLeaf';*)
    read_path t x >>= fun bs ->
    List.iter bs ~f:(List.iter ~f:(fun (a, d) -> Stash.add t.stash ~key:a ~data:d));
      (*fun ((a1,d1), (a2,d2), (a3,d3), (a4,d4)) ->
      Stash.add t.stash ~key:a1 ~data:d1;
      Stash.add t.stash ~key:a2 ~data:d2;
      Stash.add t.stash ~key:a3 ~data:d3;
      Stash.add t.stash ~key:a4 ~data:d4);*)
    (*if t.offset > 0L
      then (
        Printf.printf "After reading path, stash contains addresses:";
        Hash_set.iter ~f:(fun (a,_) -> Printf.printf " %Ld" a) t.stash;
        Printf.printf "\n");*)
    let data = match Stash.find t.stash a with
      | Some d -> d
      | None -> begin
        match op with
          | Read -> Printf.printf "Reading stash miss at address %Ld\n" a
          | Write -> Printf.printf "Writing stash miss at address %Ld\n" a
        end;
        Cstruct.create t.info.sector_size
    in
    begin match op with
      | Write -> (*Hash_set.remove t.stash (a, data);*)
                 begin match data' with
                  | Some d -> Stash.add t.stash ~key:a ~data:d; return (`Ok ())
                  | None -> return (`Error (`Unknown "Writing nothing!"))
                 end
      | Read -> return (`Ok ())
    end >>= fun () ->
    (*let take_from_stash l =
      match Hash_set.find t.stash ~f:(fun (a',_) -> Lwt_main.run (bind (P.get t.posMap a')
        (fun x' -> match x' with
          | `Ok x' -> return (Int64.(bucket_address t x l = bucket_address t x' l))
          | `Error _ -> return_false))) with
        | Some (a,d) ->
          (*if t.offset = 0L then Printf.printf "Put address %Ld in bucket at level %d\n" a l;*)
          Stash.remove t.stash a;
          (a,d)
        | None -> OBlock.dummy t.info.sector_size
    in
    let build_bucket l = (take_from_stash l, take_from_stash l, take_from_stash l, take_from_stash l) in
    let rec build_path acc = function
      | 0 -> (build_bucket 0) :: acc
      | x -> build_path ((build_bucket x) :: acc) (x - 1)
    in
    write_path t x (build_path [] t.height) >>= fun () ->*)
    let stashList = Stash.to_alist t.stash in
    let rec mapPosition acc = function
     | [] -> return (`Ok acc)
     | ((a,d) :: xs) ->
      (*if a = 0L
        then (Printf.printf "Value of address 0 in stash: "; printInts d);*)
      P.get t.posMap a >>= fun pos ->
      mapPosition ((pos,(a,d)) :: acc) xs
    in
    mapPosition [] stashList >>= fun stash ->
    write_path t x (buildPath t [] stash x t.height) >>= fun () ->
    (*if t.offset > 0L
      then (
        Printf.printf "After writing path, stash contains addresses:";
        Hash_set.iter ~f:(fun (a,_) -> Printf.printf " %Ld" a) t.stash;
        Printf.printf "\n");*)
    (*P.get t.posMap a >>= fun newLeaf'' ->
    if t.offset = 0L
      then Printf.printf "After write-back, block is at %Ld and should be at %Ld\n" newLeaf'' newLeaf;*)
    return (`Ok (data))

  let initialisePosmap t bound =
    let rec loop = function
      | 0L -> return (`Ok ())
      | x ->
        let data = Cstruct.create t.info.sector_size in
        for i = 0 to (Cstruct.len data / 8) - 1 do
          Cstruct.BE.set_uint64 data (i * 8) (Random.int64 bound);
          (*Printf.printf "%Ld " (Cstruct.BE.get_uint64 data (i * 8));*)
        done;
        (*Printf.printf "\n";*)
        access t Write Int64.(x - 1L) (Some data) >>= fun _ ->
        loop Int64.(x - 1L)
    in loop t.info.size_sectors

  let create ?(size = 0L) ?(offset = 0L) ?(bucketSize = 4L) bd =
    lwt info = B.get_info bd in
    let read_write = info.B.read_write in
    if info.B.sector_size > 8
      then (
        (* Set sector_size based on input and pad blocks so that it fits *)
        let sector_size = (info.B.sector_size - 8) + (511 * info.B.sector_size) in
        let b = Float.(to_int (log (of_int sector_size * 8.) / log 2.)) in
        (* should in fact save log size_sectors space for the stash *)
        let height =
          if size = 0L
            (* If size is unspecified, then assume non-recursive, so fill whole ORAM *)
            then floor_log Int64.(info.B.size_sectors / (512L * bucketSize) + 1L) - 1 (* sixteen temporary for testing block size *)
            else if offset > 0L
              (* Otherwise if posmap oram, then reduce size *)
              then floor_log Int64.(size / bucketSize + 1L) + 5 - b
              (* If not posmap, then calculate height normally, but from given size *)
              else floor_log Int64.(size / bucketSize + 1L) - 1
        in
        let size_sectors = Int64.(bucketSize * (pow 2L (of_int height + 1L) - 1L)) in
        (* Initialise ORAM and stash space *)
        (* Create stash *)
        let stash = Stash.create () in
        (* Create posMap *)
        P.create ~size:size_sectors ~offset:Int64.(offset + size_sectors) ~bucketSize bd >>= fun posMap ->
        let t = { info = { read_write; sector_size; size_sectors } ; height ; bucketSize ; stash ; posMap ; bd ; offset = Int64.(offset * 512L) (*); output*) } in
        (if offset > 0L
        then
          let height' = floor_log Int64.(size / bucketSize + 1L) - 1 in
          let bound = Int64.(pow 2L (of_int height')) in
          initialisePosmap t bound
        else return (`Ok ())) >>= fun () ->
        (*let output = newLog () in*)
        return (`Ok t)
      ) else return (`Error `Disconnected)

  (* Remove off + from Cstruct.copy because I think it is wrong (and it seems like I was right!) *)

  let write_to_buffer t sector_start b =
    let len = b.Cstruct.len / t.info.sector_size in
    let rec loop = function
      | 0 -> return (`Ok ())
      | x ->
        let data = Cstruct.sub b ((x - 1) * t.info.sector_size) t.info.sector_size in
        access t Write Int64.(sector_start + (of_int x) - 1L) (Some data) >>= fun _ ->
        loop (x - 1)
    in loop len

  let rec write t sector_start = function
    | [] -> return (`Ok ())
    | b :: bs ->
      if sector_start >= t.info.size_sectors
        then return (`Error (`Unknown "End of file"))
        else
          write_to_buffer t sector_start b >>= fun () ->
          write t Int64.(sector_start + (of_int b.Cstruct.len / (of_int t.info.sector_size))) bs

  let read_to_buffer t sector_start b =
    let len = b.Cstruct.len / t.info.sector_size in
    let rec loop = function
      | 0 -> return (`Ok ())
      | x ->
        access t Read Int64.(sector_start + (of_int x) - 1L) None >>= fun data ->
        Cstruct.blit data 0 b ((x - 1) * t.info.sector_size) t.info.sector_size;
        loop (x - 1)
    in loop len

  let rec read t sector_start = function
    | [] -> return (`Ok ())
    | b :: bs ->
      if sector_start >= t.info.size_sectors
        then return (`Error (`Unknown "End of file"))
        else
          read_to_buffer t sector_start b >>= fun () ->
          read t Int64.(sector_start + (of_int b.Cstruct.len / (of_int t.info.sector_size))) bs

  (* Actually perform some translation of address and extract value for single int64 from the returned bytes *)
  let get t x =
    let chi = Int64.of_int (t.info.sector_size / 8) in
    let x' = Int64.(x / chi) in
    let pos = 8 * Option.value Int64.(to_int (rem x chi)) ~default:0 in
    access t Read x' None >>= fun data ->
    let result = Cstruct.BE.get_uint64 data pos in
    (*Printf.printf "Got position of address %Ld as %Ld\n" x result;*)
    (*printInts data;*)
    return (`Ok result)

  let set t x y =
    let chi = Int64.of_int (t.info.sector_size / 8) in
    let x' = Int64.(x / chi) in
    let pos = 8 * Option.value Int64.(to_int (rem x chi)) ~default:0 in
    (*Printf.printf "Setting position of address %Ld to %Ld by updating block %Ld at position %d\n" x y x' pos;*)
    access t Read x' None >>= fun data ->
    Cstruct.BE.set_uint64 data pos y;
    (*printInts data;*)
    access t Write x' (Some data) >>= fun _ ->
    return (`Ok ())

  let length t =
    t.info.size_sectors

end
