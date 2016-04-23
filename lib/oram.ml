open Lwt
open Printf
open V1_LWT
open Core_kernel.Std
open PosMapIntf
open Bin_prot.Std

let _ = Nocrypto_entropy_lwt.initialize ()

module Make (MakePositionMap : PosMapF) (BlockDevice : BLOCK) = struct

  module PositionMap = MakePositionMap(BlockDevice)

  type 'a io = 'a Lwt.t

  type page_aligned_buffer = Cstruct.t

  type error = [
    | `Unknown of string
    | `Unimplemented
    | `Is_read_only
    | `Disconnected
    ]

  type superblock = {
      offset : int64;
      length : int;
    } [@@ deriving bin_io]

  type info = {
      read_write: bool;
      sector_size: int;
      size_sectors: int64;
    } [@@ deriving bin_io]

  type structuralInfo = {
      height : int;
      numLeaves : int64;
      sectorsPerBlock : int;
    } [@@ deriving bin_io]

  type core = {
      info : info;
      structuralInfo : structuralInfo;
      bucketSize : int64;
      offset : int64;
      desiredBlockSize : int;
      stash : Stash.t;
    } [@@ deriving bin_io]

  type t = {
      info : info;
      structuralInfo : structuralInfo;
      bucketSize : int64;
      offset : int64;
      desiredBlockSize : int;
      stash : Stash.t;
      positionMap : PositionMap.t;
      blockDevice : BlockDevice.t;
    }

  let core { info ; structuralInfo ; bucketSize ; offset ; desiredBlockSize ; stash } =
    { info ; structuralInfo ; bucketSize ; offset ; desiredBlockSize ; stash }

  let extendCore ({ info ; structuralInfo ; bucketSize ; offset ; desiredBlockSize ; stash } : core) positionMap blockDevice =
    { info ; structuralInfo ; bucketSize ; offset ; desiredBlockSize ; stash ; positionMap ; blockDevice}

  let bin_size_t t =
    let coreSize = core t |> bin_size_core in
    let posMapSize = PositionMap.bin_size_t t.positionMap in
    (*Printf.printf "%d\n" posMapSize;*)
    coreSize + posMapSize

  let bin_write_t buf ~pos t =
    let pos = core t |> bin_write_core buf ~pos in
    PositionMap.bin_write_t buf ~pos t.positionMap

  let bin_writer_t = { Bin_prot.Type_class.size = bin_size_t; write = bin_write_t  }

  let bin_read_t buf ~pos_ref blockDevice =
    let core = bin_read_core buf ~pos_ref in
    let positionMap = PositionMap.bin_read_t buf ~pos_ref blockDevice in
    extendCore core positionMap blockDevice

  type id = string

  type op =
    | Read
    | Write

  type bucket = OBlock.t list

  let sizeOfAddressInBytes = 8

  let ( >>= ) x f = x >>= function
                    | `Error e -> return (`Error e)
                    | `Ok x -> f x

  let get_info { info } = return info

  let getStructuralInfo { structuralInfo } = return structuralInfo

  let createPageAlignedBuffer bytes =
    let sizeInPages = Io_page.round_to_page_size bytes / Io_page.page_size in
    let buffer = Io_page.get_buf ~n:sizeInPages () in
    Cstruct.sub buffer 0 bytes

  let floor_log x =
    let rec loop acc = function
      | 1L -> acc
      | x -> loop (acc + 1) Int64.(x / 2L)
    in loop 0 x

  let diskSize t =
    Int64.((t.info.size_sectors * of_int t.structuralInfo.sectorsPerBlock) + (PositionMap.diskSize t.positionMap))

  let flush t =
    let%lwt info = BlockDevice.get_info t.blockDevice in
    let binarySize = bin_size_t t in
    let offset = Int64.(t.offset + diskSize t) in
    let requiredBlocks = (binarySize - 1) / info.BlockDevice.sector_size + 1 in
    let length = requiredBlocks * info.BlockDevice.sector_size in
    let superblock = { offset ; length } in
    (*let superblockBuffer = Bin_prot.Common.create_buf info.BlockDevice.sector_size in*)
    let superblockBuffer = createPageAlignedBuffer info.BlockDevice.sector_size in
    let _ = bin_write_superblock (Cstruct.to_bigarray superblockBuffer) ~pos:0 superblock in
    BlockDevice.write t.blockDevice 0L [superblockBuffer] >>= fun () ->
    (*let buffer = Bin_prot.Common.create_buf length in*)
    let buffer = createPageAlignedBuffer length in
    let _ = bin_write_t (Cstruct.to_bigarray buffer) ~pos:0 t in
    BlockDevice.write t.blockDevice offset [buffer] >>= fun () ->
    return (`Ok ())

  let connect blockDevice =
    let%lwt info = BlockDevice.get_info blockDevice in
    let superblockBuffer = createPageAlignedBuffer info.BlockDevice.sector_size in
    BlockDevice.read blockDevice 0L [superblockBuffer] >>= fun () ->
    let { offset ; length } = bin_read_superblock (Cstruct.to_bigarray superblockBuffer) ~pos_ref:(ref 0) in
    let coreBuffer = createPageAlignedBuffer length in
    BlockDevice.read blockDevice offset [coreBuffer] >>= fun () ->
    let t = bin_read_t (Cstruct.to_bigarray coreBuffer) ~pos_ref:(ref 0) blockDevice in
    return (`Ok t)

  let fakeReconnect oram blockDevice =
    return (`Ok { oram with blockDevice })

  let disconnect t =
    match%lwt flush t with
    | `Ok _ -> BlockDevice.disconnect t.blockDevice
    | `Error _ -> failwith "Failed to flush ORAM! You're probably screwed!"

  let calculateAddressOfBucket t leaf level =
    let rec loop acc = function
      | 0 -> acc
      | y ->
         if Int64.bit_and (Int64.shift_right leaf (y + (t.structuralInfo.height - level) - 1)) 1L = 1L
         then loop Int64.(2L * acc + (t.bucketSize * (of_int t.structuralInfo.sectorsPerBlock) * 2L)) (y - 1)
         else loop Int64.(2L * acc + (t.bucketSize * (of_int t.structuralInfo.sectorsPerBlock))) (y - 1)
    in loop 0L level

  let writeBucket t address bucket =
    (* Printf.printf "%Ld\n" Int64.(address + t.offset); *)
    let buffers = List.map ~f:OBlock.to_cstruct bucket in
    BlockDevice.write t.blockDevice Int64.(address + t.offset) buffers

  let writePathToLeaf t leaf path =
    let rec loop path = function
      | -1 ->
         begin match path with
         | [] -> return (`Ok ())
         | _ -> return (`Error (`Unknown "Wrong number of buckets in path"))
         end
      | y ->
         match path with
         | (bucket :: restOfPath) ->
            let addressOfBucket = calculateAddressOfBucket t leaf (t.structuralInfo.height - y) in
            writeBucket t addressOfBucket bucket >>= fun () ->
            loop restOfPath (y - 1)
         | _ -> return (`Error (`Unknown "Wrong number of buckets in path"))
    in loop path t.structuralInfo.height

  let createBuffersForBucket t =
    let zeroToBucketSize = List.range 0 (Option.value (Int64.to_int t.bucketSize) ~default:4) in
    List.map ~f:(fun _ -> createPageAlignedBuffer (t.info.sector_size + 8)) zeroToBucketSize

  let readBucket t address =
    (* Printf.printf "%Ld\n" Int64.(address + t.offset);*)
    let buffersForBucket = createBuffersForBucket t in
    BlockDevice.read t.blockDevice Int64.(address + t.offset) buffersForBucket >>= fun () ->
    let bucket = List.map ~f:OBlock.of_cstruct buffersForBucket in
    return (`Ok bucket)

  let rec readPathToLeafToLevel t leaf = function
    | -1 -> return (`Ok [])
    | level ->
       let addressOfBucket = calculateAddressOfBucket t leaf (level) in
       readBucket t addressOfBucket >>= fun bucket ->
       readPathToLeafToLevel t leaf (level - 1) >>= fun restOfPath ->
       return (`Ok (bucket :: restOfPath))

  let readPathToLeaf t leaf = readPathToLeafToLevel t leaf t.structuralInfo.height

  let validBlocks t l x = List.partition_tf ~f:(fun (pos, _) -> Int64.shift_right pos (t.structuralInfo.height - l) = Int64.shift_right x (t.structuralInfo.height - l))

  let buildBucket t choices =
    let rec loop left acc rest =
      match left, acc, rest with
      | 0L, bs, rest -> (bs, rest)
      | n, acc, [] -> loop Int64.(n - 1L) (OBlock.dummy t.info.sector_size :: acc) []
      | n, acc, ((_, (a,d)) :: xs) -> Stash.remove t.stash a; loop Int64.(n - 1L) ((a,d) :: acc) xs
    in loop t.bucketSize [] choices

  let buildPathToLeaf t leaf =
    let stashList = Stash.to_alist t.stash in
    let rec mapPosition acc = function
      | [] -> return (`Ok acc)
      | ((a,d) :: xs) ->
         PositionMap.get t.positionMap a >>= fun pos ->
         mapPosition ((pos,(a,d)) :: acc) xs
    in
    mapPosition [] stashList >>= fun stash ->
    let rec aux t acc stash x = function
      | 0 ->
         let (hits, misses) = validBlocks t 0 x stash in
         let (bucket, rest) = buildBucket t hits in
         bucket :: acc
      | l ->
         let (hits, misses) = validBlocks t l x stash in
         let (bucket, rest) = buildBucket t hits in
         aux t (bucket :: acc) (rest @ misses) x (l - 1)
    in
    return (`Ok (aux t [] stash leaf t.structuralInfo.height))

  let access t operation address dataToWrite =
    PositionMap.get t.positionMap address >>= fun leaf ->
    (*Printf.printf "%Ld\n" leaf;*)
    let newLeaf = Nocrypto.Rng.Int64.gen t.structuralInfo.numLeaves in
    PositionMap.set t.positionMap address newLeaf >>= fun () ->
    readPathToLeaf t leaf >>= fun pathRead ->
    List.iter pathRead ~f:(
                List.iter ~f:(
                            fun (address, data) ->  Stash.add t.stash ~address ~data));
    let dataRead = match Stash.find t.stash address with
      | Some data -> data
      | None -> Cstruct.create t.info.sector_size
    in
    begin match operation with
    | Write ->
       begin match dataToWrite with
       | Some data ->
          Stash.add t.stash ~address ~data;
          return (`Ok ())
       | None -> return (`Error (`Unknown "Writing nothing!"))
       end
    | Read -> return (`Ok ())
    end >>= fun () ->
    buildPathToLeaf t leaf >>= fun pathToWrite ->
    writePathToLeaf t leaf pathToWrite >>= fun () ->
    return (`Ok (dataRead))

  let initialise t =
    let dummyBuffer = createPageAlignedBuffer (t.info.sector_size + 8) in
    Cstruct.LE.set_uint64 dummyBuffer 0 (-1L);
    for i = 8 to Cstruct.len dummyBuffer - 1 do
      Cstruct.set_uint8 dummyBuffer i 0
    done;
    let rec loop = function
      | 0L -> return (`Ok ())
      | x ->
      BlockDevice.write t.blockDevice Int64.(t.offset + (x - 1L) * (of_int t.structuralInfo.sectorsPerBlock)) [dummyBuffer] >>= fun () -> loop Int64.(x - 1L)
    in
    loop t.info.size_sectors >>= fun () ->
    if t.offset > Int64.of_int t.structuralInfo.sectorsPerBlock
    then (
      let b = Float.(to_int (log (of_int t.info.sector_size * 8.) / log 2.)) in
      let height' = t.structuralInfo.height - 6 + b in
      let bound = Int64.(pow 2L (of_int height')) in
      let randomBuffer = createPageAlignedBuffer t.info.sector_size in
      let rec loop = function
        | 0L -> return (`Ok ())
        | x ->
           for i = 0 to (Cstruct.len randomBuffer / 8) - 1 do
             Cstruct.BE.set_uint64 randomBuffer (i * 8) (Nocrypto.Rng.Int64.gen bound);
           done;
           access t Write Int64.(x - 1L) (Some randomBuffer) >>= fun _ ->
           loop Int64.(x - 1L)
      in
      loop t.info.size_sectors
    ) else return (`Ok ())

  let stashSizesForBucketSize = [(4L,89L);(5L,63L);(6L,53L)]

  let calculateHeightToFillBlockDevice info sectorsPerBlock bucketSize =
    (* Need to accommodate stash space into this calculation *)
    let maxStashSizeInBlocks = List.Assoc.find_exn stashSizesForBucketSize bucketSize in
    let normalisedSizeInSectors = Int64.(info.BlockDevice.size_sectors / of_int sectorsPerBlock) in
    let sectorsForORAM = Int64.(normalisedSizeInSectors - maxStashSizeInBlocks) in
    Printf.printf "Saving %Ld of %Ld blocks for stash, leaving %Ld for ORAM\n%!" maxStashSizeInBlocks normalisedSizeInSectors sectorsForORAM;
    if sectorsForORAM > 4L
    then floor_log Int64.(sectorsForORAM / bucketSize + 1L) - 1
    else failwith "BlockDevice too small for current parameters"

  let calculateHeightOfPositionMap desiredSizeInSectors sector_size bucketSize =
    let reductionFactor = Float.(to_int (log (of_int sector_size * 8.) / log 2.)) in
    floor_log Int64.(desiredSizeInSectors / bucketSize + 1L) + 5 - reductionFactor

  let calculateHeightOfDataORAM desiredSizeInSectors bucketSize =
    floor_log Int64.(desiredSizeInSectors / bucketSize + 1L) - 1

  let calculateHeight desiredSizeInSectors info sectorsPerBlock sector_size bucketSize offset =
    if desiredSizeInSectors = 0L
    then calculateHeightToFillBlockDevice info sectorsPerBlock bucketSize
    else if offset > Int64.of_int (sector_size + sizeOfAddressInBytes)
    then calculateHeightOfPositionMap desiredSizeInSectors sector_size bucketSize
    else calculateHeightOfDataORAM desiredSizeInSectors bucketSize

  let createInstanceOfType desiredSizeInSectors bucketSize desiredBlockSize offset blockDevice =
    let%lwt info = BlockDevice.get_info blockDevice in
    let read_write = info.BlockDevice.read_write in
    let sectorsPerBlock = (desiredBlockSize - 1) / info.BlockDevice.sector_size + 1 in
    let blockOffset = Int64.(offset * (of_int sectorsPerBlock)) in
    let sector_size = sectorsPerBlock * info.BlockDevice.sector_size - sizeOfAddressInBytes in
    (* should in fact save log size_sectors space for the stash *)
    let height = calculateHeight desiredSizeInSectors info sectorsPerBlock sector_size bucketSize blockOffset in
    let numLeaves = Int64.(pow 2L (of_int height)) in
    let size_sectors = Int64.(bucketSize * (2L * numLeaves - 1L)) in
    (* let totalSizeSectors = Int64.(info.BlockDevice.size_sectors / of_int sectorsPerBlock) in *)
    let stash = Stash.create () in
    PositionMap.create ~desiredSizeInSectors:size_sectors ~bucketSize ~desiredBlockSize ~offset:Int64.(offset + size_sectors) blockDevice >>= fun positionMap ->
    let t = {
        info = { read_write; sector_size; size_sectors } ;
        structuralInfo = { height ; numLeaves ; sectorsPerBlock } ;
        bucketSize ; desiredBlockSize ; offset = blockOffset ;
        stash ; positionMap ; blockDevice
      }
    in
    (*Printf.printf "Created ORAM of size %Ld with block size %d\n%!" size_sectors sector_size;*)
    return (`Ok t)

  let create ?(desiredSizeInSectors = 0L) ?(bucketSize = 4L) ?(desiredBlockSize = 0x2000) ?(offset = 1L) blockDevice =
    let%lwt info = BlockDevice.get_info blockDevice in
    let desiredBlockSizeToUse =
      if info.BlockDevice.sector_size > desiredBlockSize
      then info.BlockDevice.sector_size
      else desiredBlockSize
    in
    (*Printf.printf "Attempting to create ORAM of size %Ld with block size %d\n%!" desiredSizeInSectors desiredBlockSizeToUse;*)
    createInstanceOfType desiredSizeInSectors bucketSize desiredBlockSizeToUse offset blockDevice >>= fun t ->
    initialise t >>= fun () ->
    flush t >>= fun () ->
    return (`Ok t)

  let writeBuffer t startAddress buffer =
    let bufferLengthInSectors = buffer.Cstruct.len / t.info.sector_size in
    let rec writeSectors = function
      | -1 -> return (`Ok ())
      | sectorAddressInBuffer ->
         let offsetInBuffer = sectorAddressInBuffer * t.info.sector_size in
         let dataToWrite = Cstruct.sub buffer offsetInBuffer t.info.sector_size in
         access t Write Int64.(startAddress + (of_int sectorAddressInBuffer)) (Some dataToWrite) >>= fun _ ->
         writeSectors (sectorAddressInBuffer - 1)
    in writeSectors (bufferLengthInSectors - 1)

  let write t startAddress buffers =
    (* Printf.printf "(2, %d)\n" (Time_ns.to_int_ns_since_epoch (Time_ns.now ())); *)
    let rec loop startAddress = function
      | [] -> return (`Ok ())
      | buffer :: buffers ->
         let startAddressOfNextBuffer = Int64.(startAddress + (of_int buffer.Cstruct.len / (of_int t.info.sector_size))) in
         if startAddressOfNextBuffer > t.info.size_sectors
         then return (`Error (`Unknown "End of file"))
         else
           writeBuffer t startAddress buffer >>= fun () ->
           loop startAddressOfNextBuffer buffers
    in
    loop startAddress buffers >>= fun () ->
    flush t

  let readBuffer t startAddress buffer =
    let bufferLengthInSectors = buffer.Cstruct.len / t.info.sector_size in
    let rec readSectors = function
      | -1 -> return (`Ok ())
      | sectorAddressInBuffer ->
         access t Read Int64.(startAddress + (of_int sectorAddressInBuffer)) None >>= fun dataRead ->
         let offsetInBuffer = sectorAddressInBuffer * t.info.sector_size in
         Cstruct.blit dataRead 0 buffer offsetInBuffer t.info.sector_size;
         readSectors (sectorAddressInBuffer - 1)
    in readSectors (bufferLengthInSectors - 1)

  let read t startAddress buffers =
    (* Printf.printf "(2, %d)\n" (Time_ns.to_int_ns_since_epoch (Time_ns.now ())); *)
    let rec loop startAddress = function
      | [] -> return (`Ok ())
      | buffer :: buffers ->
         let startAddressOfNextBuffer = Int64.(startAddress + (of_int buffer.Cstruct.len / (of_int t.info.sector_size))) in
         if startAddressOfNextBuffer > t.info.size_sectors
         then return (`Error (`Unknown "End of file"))
         else
           readBuffer t startAddress buffer >>= fun () ->
           loop startAddressOfNextBuffer buffers
    in
    loop startAddress buffers >>= fun () ->
    flush t

  let get t sectorAddress =
    let sizeOfPositionInBytes = 8 in
    let positionsPerSector = Int64.of_int (t.info.sector_size / sizeOfPositionInBytes) in
    let sectorAddressInPositionMap = Int64.(sectorAddress / positionsPerSector) in
    let offsetInSector = sizeOfPositionInBytes * Option.value Int64.(to_int (rem sectorAddress positionsPerSector)) ~default:0 in
    access t Read sectorAddressInPositionMap None >>= fun sectorRead ->
    let leaf = Cstruct.BE.get_uint64 sectorRead offsetInSector in
    return (`Ok leaf)

  let set t sectorAddress newLeaf =
    let sizeOfPositionInBytes = 8 in
    let positionsPerSector = Int64.of_int (t.info.sector_size / sizeOfPositionInBytes) in
    let sectorAddressInPositionMap = Int64.(sectorAddress / positionsPerSector) in
    let offsetInSector = sizeOfPositionInBytes * Option.value Int64.(to_int (rem sectorAddress positionsPerSector)) ~default:0 in
    access t Read sectorAddressInPositionMap None >>= fun dataRead ->
    Cstruct.BE.set_uint64 dataRead offsetInSector newLeaf;
    access t Write sectorAddressInPositionMap (Some dataRead) >>= fun _ ->
    return (`Ok ())

  let length t =
    t.info.size_sectors

end
