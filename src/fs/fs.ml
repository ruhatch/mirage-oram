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
open V1_LWT

module Make (B : BLOCK) = struct

  module I = InodeIndex.Make(B)

  type t = {
    superblock : Cstruct.t;
    freeMap : FreeMap.t; (* Store this at some location *)
    inodeIndex : I.t; (* Store this at some location *)
    bd : B.t;
    info : B.info;
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
      let buffer = Cstruct.sub t.freeMap ((n - 1) * t.info.B.sector_size) t.info.B.sector_size in
      B.write t.bd (Int64.of_int n) [buffer] >>= fun _ ->
      writeFreeMap t (n - 1)

  let flush_meta t =
    B.write t.bd 0L [t.superblock] >>= fun () ->
    let freeMapSize = Int64.to_int (Cstruct.BE.get_uint64 t.superblock 8) in
    writeFreeMap t freeMapSize

  let initialise bd =
    lwt info = B.get_info bd in
    let superblock = Cstruct.create info.B.sector_size in
    let freeMapSize = Int64.(to_int @@ add (div (sub info.B.size_sectors 1L) (of_int (info.B.sector_size * 8))) 1L) in
    let freeMap = FreeMap.create freeMapSize info.B.sector_size in
    I.create freeMap bd info.B.sector_size >>= fun inodeIndex ->
    Cstruct.BE.set_uint64 superblock 0 inodeIndex.I.rootAddress;
    Cstruct.BE.set_uint64 superblock 8 (Int64.of_int freeMapSize);
    let t = { superblock ; freeMap ; inodeIndex ; bd ; info } in
    flush_meta t >>= fun () ->
    return (`Ok t)

  let connect bd =
    lwt info = B.get_info bd in
    let freeMap = Cstruct.create info.B.sector_size in
    B.read bd 1L [freeMap] >>= fun () ->
    let superblock = Cstruct.create info.B.sector_size in
    B.read bd 0L [superblock] >>= fun () ->
    let rootAddress = Cstruct.BE.get_uint64 superblock 0 in
    I.connect freeMap bd info.B.sector_size rootAddress >>= fun inodeIndex ->
    return (`Ok { superblock ; freeMap ; inodeIndex ; bd ; info })

  let createFile t name =
    let inode = Inode.create (t.info.B.sector_size / 8) in
    let inodeNum = hash name in
    begin match FreeMap.alloc t.freeMap 1 with
      | [diskAddr] -> return (`Ok diskAddr)
      | _ -> return (`Error (`Unknown "Wrong number of blocks allocated"))
    end >>= fun diskAddr ->
    I.insert t.inodeIndex inodeNum diskAddr >>= fun () ->
    B.write t.bd diskAddr [inode] >>= fun () ->
    flush_meta t

  (* Should probably remove inode and free memory if there is an error *)

  let inodeForFile t name =
    let inodeNum = hash name in
    I.find t.inodeIndex t.inodeIndex.I.root inodeNum >>= fun a ->
    match a with
      | None -> return (`Error (`Unknown "File not found"))
      | Some diskAddr ->
        let inode = Inode.create (t.info.B.sector_size / 8) in
        B.read t.bd diskAddr [inode] >>= fun () ->
        return (`Ok inode)

  let flushInodeForFile t name inode =
    let inodeNum = hash name in
    I.find t.inodeIndex t.inodeIndex.I.root inodeNum >>= fun a ->
    match a with
      | None -> return (`Error (`Unknown "File not found"))
      | Some diskAddr ->
        B.write t.bd diskAddr [inode]

  let writeFile t name contents =
    inodeForFile t name >>= fun inode ->
    let inodeLength = Inode.noPtrs inode in
    let contentLength = (Cstruct.len contents - 1) / t.info.B.sector_size + 1 in
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
        let buffer = Cstruct.sub contents ((n - 1) * t.info.B.sector_size) t.info.B.sector_size in
        B.write t.bd (Inode.getPtr inode n) [buffer] >>= fun () ->
        loop (n - 1)
    in loop (Inode.noPtrs inode) >>= fun () ->
    flush_meta t

  (* Need length or EOF markers so that we don't need sector_size aligned files*)
  let readFile t name =
    inodeForFile t name >>= fun inode ->
    let inodeLength = Inode.noPtrs inode in
    let result = Cstruct.create (inodeLength * t.info.B.sector_size) in
    let rec loop = function
      | 0 -> return (`Ok ())
      | n ->
        let buffer = Cstruct.sub result ((n - 1) * t.info.B.sector_size) t.info.B.sector_size in
        B.read t.bd (Inode.getPtr inode n) [buffer] >>= fun () ->
        loop (n - 1)
    in loop (Inode.noPtrs inode) >>= fun () ->
    return (`Ok result)

end
