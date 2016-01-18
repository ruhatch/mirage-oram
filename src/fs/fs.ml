(**

  Simple Filesystem

  Sector 0 : Superblock (contains the root address for the InodeIndex and the size of the FreeMap)
  Sector 1 : FreeMap (Bitarray of free memory locations)
      .
      .
      .
  Sector n : FreeMap
  Sector n + 1 : InodeIndex Root
      .
      .
      .
  Sector m : Other Blocks

*)

open Lwt

module type S = sig

  type blockDevice
  type blockDeviceInfo
  type error
  type inodeIndex

  type t = {
    superblock : Cstruct.t;
    freeMap : FreeMap.t;
    inodeIndex : inodeIndex;
    blockDevice : blockDevice;
    info : blockDeviceInfo;
  }

  val initialise : blockDevice -> [`Error of error | `Ok of t] Lwt.t

  val connect : blockDevice -> [`Error of error | `Ok of t] Lwt.t

  (* val createFile : t -> string -> [`Error of error | `Ok of unit] Lwt.t *)

  val fileExists : t -> string -> [`Error of error | `Ok of bool] Lwt.t

  val writeFile : t -> string -> Cstruct.t -> [`Error of error | `Ok of unit] Lwt.t

  val readFile : t -> string -> [`Error of error | `Ok of Cstruct.t] Lwt.t

end

module Make (BlockDevice : V1_LWT.BLOCK) = struct

  module I = InodeIndex.Make(BlockDevice)

  type blockDevice = BlockDevice.t
  type blockDeviceInfo = BlockDevice.info
  type error = BlockDevice.error
  type inodeIndex = I.t

  type t = {
    superblock : Cstruct.t;
    freeMap : FreeMap.t;
    inodeIndex : inodeIndex;
    blockDevice : blockDevice;
    info : blockDeviceInfo;
  }

  let ( >>= ) x f = x >>= function
    | `Error e -> return (`Error e)
    | `Ok x -> f x

  let hash name =
    let primary = Hashtbl.hash name in
    let secondary = primary lxor (primary lsr 21) lxor (primary lsr 17) lxor (primary lsr 48) in
    secondary land 65535

  let rec writeFreeMap t = function
    | 0 -> return (`Ok ())
    | n ->
      let buffer = Cstruct.sub t.freeMap ((n - 1) * t.info.BlockDevice.sector_size) t.info.BlockDevice.sector_size in
      BlockDevice.write t.blockDevice (Int64.of_int n) [buffer] >>= fun _ ->
      writeFreeMap t (n - 1)

  let flush_meta t =
    BlockDevice.write t.blockDevice 0L [t.superblock] >>= fun () ->
    let freeMapSize = Int64.to_int (Cstruct.BE.get_uint64 t.superblock 8) in
    writeFreeMap t freeMapSize

  let initialise blockDevice =
    lwt info = BlockDevice.get_info blockDevice in
    let superblock = Cstruct.create info.BlockDevice.sector_size in
    let freeMapSize = Int64.(to_int @@ add (div (sub info.BlockDevice.size_sectors 1L) (of_int (info.BlockDevice.sector_size * 8))) 1L) in
    let freeMap = FreeMap.create freeMapSize info.BlockDevice.sector_size in
    I.create freeMap blockDevice info.BlockDevice.sector_size >>= fun inodeIndex ->
    Cstruct.BE.set_uint64 superblock 0 inodeIndex.I.rootAddress;
    Cstruct.BE.set_uint64 superblock 8 (Int64.of_int freeMapSize);
    let t = { superblock ; freeMap ; inodeIndex ; blockDevice ; info } in
    flush_meta t >>= fun () ->
    return (`Ok t)

  let connect blockDevice =
    lwt info = BlockDevice.get_info blockDevice in
    let freeMap = Cstruct.create info.BlockDevice.sector_size in
    BlockDevice.read blockDevice 1L [freeMap] >>= fun () ->
    let superblock = Cstruct.create info.BlockDevice.sector_size in
    BlockDevice.read blockDevice 0L [superblock] >>= fun () ->
    let rootAddress = Cstruct.BE.get_uint64 superblock 0 in
    I.connect freeMap blockDevice info.BlockDevice.sector_size rootAddress >>= fun inodeIndex ->
    return (`Ok { superblock ; freeMap ; inodeIndex ; blockDevice ; info })

  let createFile t name =
    let inode = Inode.create (t.info.BlockDevice.sector_size / 8) in
    let inodeNum = hash name in
    begin match FreeMap.alloc t.freeMap 1 with
      | [diskAddr] -> return (`Ok diskAddr)
      | _ -> return (`Error (`Unknown "Wrong number of blocks allocated"))
    end >>= fun diskAddr ->
    I.insert t.inodeIndex inodeNum diskAddr >>= fun () ->
    BlockDevice.write t.blockDevice diskAddr [inode] >>= fun () ->
    flush_meta t >>= fun () ->
    return (`Ok inode)

  (* Should probably remove inode and free memory if there is an error *)

  let inodeForFile t name =
    let inodeNum = hash name in
    I.find t.inodeIndex t.inodeIndex.I.root inodeNum >>= fun a ->
    match a with
      | None -> return (`Ok None)
      | Some diskAddr ->
        let inode = Inode.create (t.info.BlockDevice.sector_size / 8) in
        BlockDevice.read t.blockDevice diskAddr [inode] >>= fun () ->
        return (`Ok (Some inode))

  let fileExists t name =
    inodeForFile t name >>= fun inode ->
    match inode with
      | Some _ -> return (`Ok true)
      | None -> return (`Ok false)

  let flushInodeForFile t name inode =
    let inodeNum = hash name in
    I.find t.inodeIndex t.inodeIndex.I.root inodeNum >>= fun a ->
    match a with
      | None -> return (`Error (`Unknown "File not found"))
      | Some diskAddr ->
        BlockDevice.write t.blockDevice diskAddr [inode]

  let writeFile t name contents =
    inodeForFile t name >>= fun inode ->
    begin match inode with
      | Some inode -> return (`Ok inode)
      | None -> createFile t name
    end >>= fun inode ->
    let inodeLength = Inode.noPtrs inode in
    let contentLength = (Cstruct.len contents - 1) / t.info.BlockDevice.sector_size + 1 in
    if contentLength < inodeLength
    then
      let freed = Inode.prunePtrs inode contentLength in
      FreeMap.free t.freeMap freed
    else if contentLength > inodeLength
    then (
      let allocated = FreeMap.alloc t.freeMap (contentLength - inodeLength) in
      Inode.addPtrs inode allocated
    );
    flushInodeForFile t name inode >>= fun () ->
    let rec loop = function
      | 0 -> return (`Ok ())
      | n ->
        let buffer = Cstruct.sub contents ((n - 1) * t.info.BlockDevice.sector_size) t.info.BlockDevice.sector_size in
        BlockDevice.write t.blockDevice (Inode.getPtr inode n) [buffer] >>= fun () ->
        loop (n - 1)
    in loop (Inode.noPtrs inode) >>= fun () ->
    flush_meta t

  (* Need length or EOF markers so that we don't need sector_size aligned files*)
  let readFile t name =
    inodeForFile t name >>= fun inode ->
    begin match inode with
      | Some inode -> return (`Ok inode)
      | None -> return (`Error (`Unknown (Printf.sprintf "File %s does not exist" name)))
    end >>= fun inode ->
    let inodeLength = Inode.noPtrs inode in
    let result = Cstruct.create (inodeLength * t.info.BlockDevice.sector_size) in
    let rec loop = function
      | 0 -> return (`Ok ())
      | n ->
        let buffer = Cstruct.sub result ((n - 1) * t.info.BlockDevice.sector_size) t.info.BlockDevice.sector_size in
        BlockDevice.read t.blockDevice (Inode.getPtr inode n) [buffer] >>= fun () ->
        loop (n - 1)
    in loop (Inode.noPtrs inode) >>= fun () ->
    return (`Ok result)

end
