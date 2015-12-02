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
    output : Out_channel.t;
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
      | x -> ignore_result (B.write bd Int64.(info.B.size_sectors - x) [dummy_struct]); loop Int64.(x - 1L)
    in
    loop info.B.size_sectors >>= fun () ->
      let buf = Cstruct.create info.B.sector_size in
      B.read bd 1L [buf] >>= fun () -> return (`Ok ())

  let connect bd =
    initialise bd >>= fun () ->
      lwt info = B.get_info bd in
      let read_write = info.B.read_write in
      if info.B.sector_size > 8
      then let sector_size = info.B.sector_size - 8 in
        let height = floor_log Int64.(info.B.size_sectors / 4L + 1L) - 1 in
        let size_sectors = Int64.(4L * (pow 2L (of_int height + 1L) - 1L)) in
        let stash = Stash.create () in
        let position = PosMap.create size_sectors in
        let time = Unix.gmtime (Unix.time ()) in
        let twoDigit i = if i > 9 then Printf.sprintf "%d" i else Printf.sprintf "0%d" i in
        let filename = Printf.sprintf "oramOutput%d%d%d_%s%s%s" time.Unix.tm_mday time.Unix.tm_mon time.Unix.tm_year (twoDigit time.Unix.tm_hour) (twoDigit time.Unix.tm_min) (twoDigit time.Unix.tm_sec) in
        let output = Out_channel.create filename in
        Random.self_init ();
        return (`Ok { info = { read_write; sector_size; size_sectors } ; height ; stash ; position ; bd ; output })
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
          | (b::bs) -> write_bucket t (bucket_address x (t.height - y)) b >>= fun () -> loop bs (y - 1)
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
    B.read t.bd a [buf1;buf2;buf3;buf4] >>=
    fun () ->
      return (`Ok (OBlock.of_string @@ to_string buf1,
                   OBlock.of_string @@ to_string buf2,
                   OBlock.of_string @@ to_string buf3,
                   OBlock.of_string @@ to_string buf4))

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
    (*Printf.printf "%s at block %d\n" (match op with | Read -> "Reading" | Write -> "Writing") (Option.value ~default:0 @@ Int64.to_int a);*)
    let x = PosMap.get t.position a in
    Out_channel.output_string t.output @@ Printf.sprintf "%Ld" x;
    Out_channel.newline t.output;
    Out_channel.flush t.output;
    PosMap.set t.position a @@ Random.int64 Int64.(((t.info.size_sectors / 4L) + 1L) / 2L);
    (*Printf.printf "Remapped block %d to position %d\n" (Option.value ~default:0 @@ Int64.to_int a) (Option.value ~default:0 @@ Int64.to_int @@ PosMap.get t.position a);*)
    read_path t x >>=
    fun bs ->
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
      write_path t x (build_path [] t.height) >>= fun () -> return (`Ok (data))

  (* Remove off + from Cstruct.copy because I think it is wrong (and it seems like I was right!) *)

  let write_to_buffer t sector_start b =
    let off = b.Cstruct.off in
    let len = b.Cstruct.len / t.info.sector_size in
    let rec loop = function
      | 0 -> return (`Ok ())
      | x ->
        let data = Cstruct.copy b ((x - 1) * t.info.sector_size) t.info.sector_size in
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
    let off = b.Cstruct.off in
    let len = b.Cstruct.len / t.info.sector_size in
    let rec loop = function
      | 0 -> return (`Ok ())
      | x ->
        access t Read Int64.(sector_start + (of_int x) - 1L) None >>=
        fun data ->
          Cstruct.blit_from_string data 0 b ((x - 1) * t.info.sector_size) t.info.sector_size;
          loop (x - 1)
    in loop len

  let rec read t sector_start = function
    | [] -> return (`Ok ())
    | b :: bs ->
      if sector_start >= t.info.size_sectors
      then return (`Error (`Unknown "End of file"))
      else read_to_buffer t sector_start b >>=
        fun () -> read t Int64.(sector_start + (of_int b.Cstruct.len / (of_int t.info.sector_size))) bs

end
