open Lwt
open Printf
open V1_LWT
open Core_kernel.Std

module Make (PF: PosMap.POSMAP)(B: BLOCK) = struct

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

  type bucket = OBlock.t * OBlock.t * OBlock.t * OBlock.t

  let ( >>= ) x f = x >>= function
    | `Error e -> return (`Error e)
    | `Ok x -> f x

  let get_info { info } = return info

  let floor_log x =
    let rec loop acc = function
      | 1L -> acc
      | x -> loop (acc + 1) Int64.(x / 2L)
    in loop 0 x

  let initialise bd =
    lwt info = B.get_info bd in
    let dummy_struct = Cstruct.create info.B.sector_size in
    Cstruct.LE.set_uint64 dummy_struct 0 (-1L);
    (*Printf.printf "Initialising all block addresses to %d\n" (Option.value ~default:0 @@ Int64.to_int @@ Cstruct.LE.get_uint64 dummy_struct 0);*)
    for i = 8 to Cstruct.len dummy_struct - 1 do
      Cstruct.set_uint8 dummy_struct i 0
    done;
    let rec loop = function
      | 0L -> return (`Ok ())
      | x -> B.write bd Int64.(info.B.size_sectors - x) [dummy_struct] >>= fun () -> loop Int64.(x - 1L)
    in
    loop info.B.size_sectors >>= fun () ->
    let buf = Cstruct.create info.B.sector_size in
    B.read bd 1L [buf] >>= fun () -> return (`Ok ())

  (*let newLog () =
    let time = Unix.gmtime (Unix.time ()) in
    let twoDigit i = if i > 9 then Printf.sprintf "%d" i else Printf.sprintf "0%d" i in
    let filename = Printf.sprintf "oramOutput%d%d%d_%s%s%s" time.Unix.tm_mday time.Unix.tm_mon time.Unix.tm_year (twoDigit time.Unix.tm_hour) (twoDigit time.Unix.tm_min) (twoDigit time.Unix.tm_sec) in
    let output = Out_channel.create filename in
    output*)

  let connect bd =
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
      else return (`Error `Disconnected)

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
    (*Printf.printf "Writing bucket at physical address %Ld from ORAM at offset %Ld\n" Int64.(a + t.offset) t.offset;*)
    let buf1 = OBlock.to_cstruct b1 in
    let buf2 = OBlock.to_cstruct b2 in
    let buf3 = OBlock.to_cstruct b3 in
    let buf4 = OBlock.to_cstruct b4 in
    B.write t.bd Int64.(a + t.offset) [buf1;buf2;buf3;buf4]

  let write_path t x bs =
    let rec loop bs = function
      | 0 ->
        begin match bs with
          | [b] -> write_bucket t (bucket_address x t.height) b
          | _ -> return (`Error (`Unknown "Wrong number of buckets in path"))
        end
      | y ->
        match bs with
          | (b::bs) -> write_bucket t (bucket_address x (t.height - y)) b >>= fun () -> loop bs (y - 1)
          | _ -> return (`Error (`Unknown "Wrong number of buckets in path"))
    in loop bs t.height

  let read_bucket t a =
    (*Printf.printf "Reading bucket at physical address %Ld from ORAM at offset %Ld\n" Int64.(a + t.offset) t.offset;*)
    let sector_size = t.info.sector_size + 8 in
    let open Cstruct in
    let buf1 = create sector_size in
    let buf2 = create sector_size in
    let buf3 = create sector_size in
    let buf4 = create sector_size in
    B.read t.bd Int64.(a + t.offset) [buf1;buf2;buf3;buf4] >>=
    fun () ->
      return (`Ok (OBlock.of_cstruct buf1,
                   OBlock.of_cstruct buf2,
                   OBlock.of_cstruct buf3,
                   OBlock.of_cstruct buf4))

  let read_path t x =
    let rec loop = function
      | 0 ->
        read_bucket t (bucket_address x 0) >>=
        fun b -> return (`Ok ([b]))
      | y ->
        read_bucket t (bucket_address x y) >>=
        fun b ->
          loop (y - 1) >>=
          fun bs -> return (`Ok (b::bs))
    in loop t.height

  let access t op a data' =
    (*Printf.printf "%s at block %Ld\n" (match op with | Read -> "Reading" | Write -> "Writing") a;*)
    P.get t.posMap a >>= fun x ->
    (*Out_channel.output_string t.output @@ Printf.sprintf "Oram at offset %Ld accessing path to leaf %Ld" t.offset x;
    Out_channel.newline t.output;
    Out_channel.flush t.output;*)
    P.set t.posMap a @@ Random.int64 Int64.(((t.info.size_sectors / 4L) + 1L) / 2L) >>= fun () ->
    (*Printf.printf "Remapped block %Ld to position %Ld\n" a (PosMap.get t.posMap a);*)
    read_path t x >>=
    fun bs ->
      (*Printf.printf "Successfully read path to leaf %Ld\n" x;*)
      List.iter bs ~f:(fun (b1, b2, b3, b4) ->
        (*match b1, b2, b3, b4 with
          | (a1,_), (a2,_), (a3,_), (a4,_) ->
            Printf.printf "Added blocks with the following addresses to the stash: %Ld %Ld %Ld %Ld\n" a1 a2 a3 a4;*)
        Stash.add t.stash b1;
        Stash.add t.stash b2;
        Stash.add t.stash b3;
        Stash.add t.stash b4);
      let data = match Stash.find_index t.stash a with
        | Some (_,d) -> d
        | None -> Cstruct.create t.info.sector_size
      in
      (*Printf.printf "Found data in the stash for address %Ld as %s\n" a data;*)
      begin match op with
        | Write -> Hash_set.remove t.stash (a, data);
                   Stash.add t.stash (a, Option.value data' ~default:(Cstruct.create 0))
        | Read -> ()
      end;
      let take_from_stash l =
        match Hash_set.find t.stash ~f:(fun (a',_) -> Lwt_main.run (bind (P.get t.posMap a')
          (fun x' -> match x' with
            | `Ok x' -> return (Int64.(bucket_address x l = bucket_address x' l))
            | `Error _ -> return_false))) with
          | Some b -> Hash_set.remove t.stash b;
                      b
          | None -> OBlock.dummy t.info.sector_size
      in
      let build_bucket l = (take_from_stash l, take_from_stash l, take_from_stash l, take_from_stash l) in
      let rec build_path acc = function
        | 0 -> (build_bucket 0) :: acc
        | x -> build_path ((build_bucket x) :: acc) (x - 1)
      in
      write_path t x (build_path [] t.height) >>= fun () ->
      return (`Ok (data))

  let initialisePosmap t =
    let bound = Int64.(((t.info.size_sectors / 4L) + 1L) / 2L) in
    let rec loop = function
      | 0L -> return (`Ok ())
      | x ->
        let data = Cstruct.create t.info.sector_size in
        for i = 0 to (Cstruct.len data / 8) - 1 do
          Cstruct.BE.set_uint64 data (i * 8) (Random.int64 bound);
        done;
        access t Write Int64.(x - 1L) (Some data) >>= fun _ ->
        loop Int64.(x - 1L)
    in loop t.info.size_sectors

  let create ?(size = 0L) ?(offset = 0L) bd =
    (* Calculate size based on height *)
    lwt info = B.get_info bd in
    let read_write = info.B.read_write in
    if info.B.sector_size > 8
      then (
        let sector_size = info.B.sector_size - 8 in
        let b = Float.(to_int (log (of_int sector_size * 8.) / log 2.)) in
        (* should in fact save log size_sectors space for the stash *)
        let height =
          if size = 0L
            (* If size is unspecified, then assume non-recursive, so fill whole ORAM *)
            then floor_log Int64.(info.B.size_sectors / 4L + 1L) - 1
            else if offset > 0L
              (* Otherwise if posmap oram, then reduce size *)
              then floor_log Int64.(size / 4L + 1L) + 5 - b
              (* If not posmap, then calculate height normally, but from given size *)
              else floor_log Int64.(size / 4L + 1L) - 1
        in
        let size_sectors = Int64.(4L * (pow 2L (of_int height + 1L) - 1L)) in
        (* Initialise ORAM and stash space *)
        (* Create stash *)
        let stash = Stash.create () in
        (* Create posMap *)
        P.create ~size:size_sectors ~offset:Int64.(offset + size_sectors) bd >>= fun posMap ->
        let t = { info = { read_write; sector_size; size_sectors } ; height ; stash ; posMap ; bd ; offset (*); output*) } in
        (if offset > 0L
        then initialisePosmap t
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
        access t Write Int64.(sector_start + (of_int x) - 1L) (Some data) >>=
        fun _ -> loop (x - 1)
    in loop len

  let rec write t sector_start = function
    | [] -> return (`Ok ())
    | b :: bs ->
      if sector_start >= t.info.size_sectors
      then return (`Error (`Unknown "End of file"))
      else write_to_buffer t sector_start b >>=
        fun () -> write t Int64.(sector_start + (of_int b.Cstruct.len / (of_int t.info.sector_size))) bs

  let read_to_buffer t sector_start b =
    let len = b.Cstruct.len / t.info.sector_size in
    let rec loop = function
      | 0 -> return (`Ok ())
      | x ->
        access t Read Int64.(sector_start + (of_int x) - 1L) None >>=
        fun data ->
          Cstruct.blit data 0 b ((x - 1) * t.info.sector_size) t.info.sector_size;
          loop (x - 1)
    in loop len

  let rec read t sector_start = function
    | [] -> return (`Ok ())
    | b :: bs ->
      if sector_start >= t.info.size_sectors
      then return (`Error (`Unknown "End of file"))
      else read_to_buffer t sector_start b >>=
        fun () -> read t Int64.(sector_start + (of_int b.Cstruct.len / (of_int t.info.sector_size))) bs

  (* Actually perform some translation of address and extract value for single int64 from the returned bytes *)
  let get t x =
    let chi = Int64.of_int (t.info.sector_size / 8) in
    let x' = Int64.(x / chi) in
    let pos = Option.value Int64.(to_int (rem x chi)) ~default:0 in
    access t Read x' None >>= fun data ->
    return (`Ok (Cstruct.BE.get_uint64 data pos))

  let set t x y =
    let chi = Int64.of_int (t.info.sector_size / 8) in
    let x' = Int64.(x / chi) in
    let pos = Option.value Int64.(to_int (rem x chi)) ~default:0 in
    access t Read x' None >>= fun data ->
    Cstruct.BE.set_uint64 data pos y;
    access t Write x' (Some data) >>= fun _ ->
    return (`Ok ())

  let length t =
    t.info.size_sectors

end
