(**

  Simple Filesystem

  Sector 0 : Superblock (contains rootAddress for the InodeIndex and maybe some other stuff...we'll see)
  Sector 1 : FreeMap (Bitarray of free memory locations)
  Sector 2 : InodeIndex (Mappings from inode numbers to disk addresses)
      .
      .
      .
  Sector n : Other files!

  We need to decide on how many sectors to use for InodeIndex

  If sector size is x bytes, then can address x / 8 inodes in one sector

  Each inode can then adress x / 8 locations, giving x^2 / 64
  addressible locations per sector of index

  If disk is of size b sectors,
  then to cover whole disk, we need
  64 * b / x ^ 2 sectors of InodeIndex

  For disk of size 2GB and 512B sector size,
  this gives 954 sectors of InodeIndex...

  However on a disk of 256MB, this is only 123 sectors

  We would want to selectively load the index from memory probably, not the whole thing...
  Or in fact load the whole thing, but just store the individual lines when they change!

  File names also present an issue

  If we have ~7000 inodes each with a 32B filename, that is ~400 sectors dedicated to filenames

  Scanning this would be a hassle probably

  But lets do this inefficiently at first and we can rethink later

  This is sounding a lot like one of those vEB trees or something similar...

  It's a B-Tree! Think about building the whole filesystem as a FreeMap and a B-Tree

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

  (* Flush freeMap to disk *)
  (* Flush inodeIndex rootAddress to disk *)
  (* Everything else can be reconstructed *)
  let flush_meta t =
    B.write t.bd 0L [t.superblock]

  (* Extract info from block device *)
  (* Create relevant data structures of right size *)
  (* Write those out to disk *)
  let initialise bd =
    lwt info = B.get_info bd in
    let superblock = Cstruct.create info.B.sector_size in
    let freeMap = FreeMap.create (Int64.to_int info.B.size_sectors) in (* Unsafe, need to change the structure of the freeMap *)
    I.create freeMap bd info.B.sector_size >>= fun inodeIndex ->
    let t = { superblock ; freeMap ; inodeIndex ; bd ; info } in
    flush_meta t >>= fun () ->
    return (`Ok t)

  (* Read freeMap from memory *)
  (* Read root of index from memory and construct InodeIndex *)
  let connect bd =
    lwt info = B.get_info bd in
    let freeMap = FreeMap.create (Int64.to_int info.B.size_sectors) in (* Actually need to read freeMap from memory *)
    let superblock = Cstruct.create info.B.sector_size in
    B.read bd 0L [superblock] >>= fun () ->
    let rootAddress = Cstruct.BE.get_uint64 superblock 0 in
    I.connect freeMap bd info.B.sector_size rootAddress >>= fun inodeIndex ->
    return (`Ok { superblock ; freeMap ; inodeIndex ; bd ; info })

  let createFile t name =
    Printf.printf "*** CREATING FILE ***\n";
    let inode = Inode.create (t.info.B.sector_size / 8) in
    let inodeNum = hash name in
    let [diskAddr] = FreeMap.alloc t.freeMap 1 in
    Printf.printf "About to insert key\n";
    I.insert t.inodeIndex inodeNum diskAddr >>= fun () ->
    Printf.printf "Inserted key\n";
    B.write t.bd diskAddr [inode] >>= fun () ->
    Printf.printf "Wrote inode to disk!\n";
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
        Printf.printf "Read inode: %s\n" (Cstruct.to_string inode);
        return (`Ok inode)

  let flushInodeForFile t name inode =
    Printf.printf "Writing inode: %s\n" (Cstruct.to_string inode);
    let inodeNum = hash name in
    I.find t.inodeIndex t.inodeIndex.I.root inodeNum >>= fun a ->
    match a with
      | None -> return (`Error (`Unknown "File not found"))
      | Some diskAddr ->
        B.write t.bd diskAddr [inode]

  (*let read_file t name =
    let inode = inodeForFile t name in
    let inodeLength = Inode.length inode in
    let result = Cstruct.create (inodeLength * t.info.sector_size) in
    for i = 0 to inodeLength - 1 do
      B.read t.bd (Inode.getPtr inode i) [(Cstruct.sub result (i * t.info.sector_size) t.info.sector_size)];
    done;
    result*)

  let writeFile t name contents =
    Printf.printf "*** WRITING FILE ***\n";
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
    (* Loop around writing contents to pointers *)
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
    Printf.printf "*** READING FILE ***\n";
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
