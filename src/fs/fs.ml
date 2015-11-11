(**

  Simple Filesystem

  Sector 1 : FreeMap (Bitarray of free memory locations)
  Sector 2 : InodeIndex (Mappings from inode numbers to disk addresses)
  Sector 3 : FileMap (Mappings from names to inode numbers)
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

type t = {
  freeMap : FreeMap.t;
  inodeIndex : InodeIndex.t;
  fileMap : FileMap.t;
  bd : B.t;
  info : B.info;
}

let flush_meta t =
  (* Flush all metadata to disk *)
  t

let initialise bd =
  (* Extract info from block device *)
  (* Create relevant data structures of right size *)
  (* Write those out to disk *)
  let freeMap = FreeMap.create () in
  let inodeIndex = InodeIndex.create () in
  let fileMap = FileMap.create () in
  let t = { freeMap ; inodeIndex ; fileMap ; bd } in
  flush t;
  t

let create_file t name ?contents =
  let inode = Inode.create (t.info.sector_size / 8) in
  let inodeNum = InodeIndex.nextFree t.inodeIndex in
  let diskAddr = FreeMap.alloc t.freeMap 1 in
  InodeIndex.set t.inodeIndex inodeNum diskAddr;
  FileMap.add t.fileMap name inodeNum;
  B.write t.bd diskAddr inode;
  flush_meta t;
  t

let inodeForFile t name =
  let inodeNum = FileMap.get t.fileMap name in
  let diskAddr = InodeIndex.get t.inodeIndex inodeNum in
  let inode = Inode.create length in
  B.read t.bd diskAddr inode;
  inode

let read_file t name =
  let inode = inodeForFile t name in
  let inodeLength = Inode.length inode in
  let result = Cstruct.create (inodeLength * t.info.sector_size) in
  for i = 0 to inodeLength - 1 do
    B.read t.bd (Inode.getPtr inode i) [(Cstruct.sub result (i * t.info.sector_size) t.info.sector_size)];
  done;
  result

let write_file t name contents =
  let inode = inodeForFile t name in
  let inodeLength = Inode.length inode in
  let contentLength = Cstruct.len contents in
  if contentLength < inodeLength
  then
    let ptrs = Inode.prunePtrs inode contentLength in
    FreeMap.free ptrs;
  else if contentLength > inodeLength
  then
    let ptrs = FreeMap.alloc t.freeMap (contentLength - inodeLength) in
    Inode.addPtrs ptrs;
  else ();
  (* Write subpart of Cstruct to disk *);
  flush_meta t;
  t
