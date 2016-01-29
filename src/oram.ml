open Lwt
open Printf
open V1_LWT
open Core_kernel.Std
open PosMapIntf

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

  type info = {
      read_write: bool;
      sector_size: int;
      size_sectors: int64;
    }

  type structuralInfo = {
      height : int;
      numLeaves : int64;
      sectorsPerBlock : int;
    }

  type t = {
      info : info;
      structuralInfo : structuralInfo;
      bucketSize : int64;
      offset : int64;
      desiredBlockSize : int;
      stash : Stash.t;
      positionMap : PositionMap.t;
      blockDevice : BlockDevice.t;
      (*output : Out_channel.t;*)
    }

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

  let floor_log x =
    let rec loop acc = function
      | 1L -> acc
      | x -> loop (acc + 1) Int64.(x / 2L)
    in loop 0 x

  (*let newLog () =
    let time = Unix.gmtime (Unix.time ()) in
    let twoDigit i = if i > 9 then Printf.sprintf "%d" i else Printf.sprintf "0%d" i in
    let filename = Printf.sprintf "oramOutput%d%d%d_%s%s%s" time.Unix.tm_mday time.Unix.tm_mon time.Unix.tm_year (twoDigit time.Unix.tm_hour) (twoDigit time.Unix.tm_min) (twoDigit time.Unix.tm_sec) in
    let output = Out_channel.create filename in
    output*)

  (*let connect bd =
    lwt info = B.get_info bd in
    let read_write = info.B.read_write in
    if info.B.sector_size > 8
    then let sector_size = info.B.sector_size - 8 in
      let height = floor_log Int64.(info.B.size_sectors / 4L + 1L) - 1 in
      let size_sectors = Int64.(4L * (pow 2L (of_int height + 1L) - 1L)) in
      let stash = Stash.create () in
      P.create ~size:size_sectors bd >>= fun posMap ->
      (*let output = newLog () in*)
      return (`Ok { info = { read_write; sector_size; size_sectors } ; height ; stash ; posMap ; bd ; offset = 0L (*); output*) })
    else return (`Error `Disconnected)*)

  let fakeReconnect oram blockDevice =
    return (`Ok { oram with blockDevice })

  let disconnect t =
    BlockDevice.disconnect t.blockDevice

  let calculateAddressOfBucket t leaf level =
    let rec loop acc = function
      | 0 -> acc
      | y ->
         if Int64.bit_and (Int64.shift_right leaf (y + (t.structuralInfo.height - level) - 1)) 1L = 1L
         then loop Int64.(2L * acc + (t.bucketSize * (of_int t.structuralInfo.sectorsPerBlock) * 2L)) (y - 1)
         else loop Int64.(2L * acc + (t.bucketSize * (of_int t.structuralInfo.sectorsPerBlock))) (y - 1)
    in loop 0L level

  let writeBucket t address bucket =
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
    let sector_size = t.info.sector_size + 8 in
    let zeroToBucketSize = List.range 0 (Option.value (Int64.to_int t.bucketSize) ~default:4) in
    List.map ~f:(fun _ -> Cstruct.create sector_size) zeroToBucketSize

  let readBucket t address =
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
    let newLeaf = Random.int64 t.structuralInfo.numLeaves in
    PositionMap.set t.positionMap address newLeaf >>= fun () ->
    readPathToLeaf t leaf >>= fun pathRead ->
    List.iter pathRead ~f:(
                List.iter ~f:(
                            fun (address, data) -> Stash.add t.stash ~address ~data));
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
    let dummy_struct = Cstruct.create (t.info.sector_size + 8) in
    Cstruct.LE.set_uint64 dummy_struct 0 (-1L);
    for i = 8 to Cstruct.len dummy_struct - 1 do
      Cstruct.set_uint8 dummy_struct i 0
    done;
    let rec loop = function
      | 0L -> return (`Ok ())
      | x -> BlockDevice.write t.blockDevice Int64.(t.offset + (x - 1L) * (of_int t.structuralInfo.sectorsPerBlock)) [dummy_struct] >>= fun () -> loop Int64.(x - 1L)
    in
    loop t.info.size_sectors >>= fun () ->
    if t.offset > 0L
    then (
      let b = Float.(to_int (log (of_int t.info.sector_size * 8.) / log 2.)) in
      let height' = t.structuralInfo.height - 6 + b in
      let bound = Int64.(pow 2L (of_int height')) in
      let rec loop = function
        | 0L -> return (`Ok ())
        | x ->
           let data = Cstruct.create t.info.sector_size in
           for i = 0 to (Cstruct.len data / 8) - 1 do
             Cstruct.BE.set_uint64 data (i * 8) (Random.int64 bound);
           done;
           access t Write Int64.(x - 1L) (Some data) >>= fun _ ->
           loop Int64.(x - 1L)
      in
      loop t.info.size_sectors
    ) else return (`Ok ())

  let calculateHeightToFillBlockDevice info sectorsPerBlock bucketSize =
    floor_log Int64.(info.BlockDevice.size_sectors / ((of_int sectorsPerBlock) * bucketSize) + 1L) - 1

  let calculateHeightOfPositionMap desiredSizeInSectors sector_size bucketSize =
    let reductionFactor = Float.(to_int (log (of_int sector_size * 8.) / log 2.)) in
    floor_log Int64.(desiredSizeInSectors / bucketSize + 1L) + 5 - reductionFactor

  let calculateHeightOfDataORAM desiredSizeInSectors bucketSize =
    floor_log Int64.(desiredSizeInSectors / bucketSize + 1L) - 1

  let calculateHeight desiredSizeInSectors info sectorsPerBlock sector_size bucketSize offset =
    if desiredSizeInSectors = 0L
    then calculateHeightToFillBlockDevice info sectorsPerBlock bucketSize
    else if offset > 0L
    then calculateHeightOfPositionMap desiredSizeInSectors sector_size bucketSize
    else calculateHeightOfDataORAM desiredSizeInSectors bucketSize

  let createInstanceOfType desiredSizeInSectors bucketSize desiredBlockSize offset blockDevice =
    lwt info = BlockDevice.get_info blockDevice in
      let read_write = info.BlockDevice.read_write in
      let sectorsPerBlock = (desiredBlockSize - 1) / info.BlockDevice.sector_size + 1 in
      let blockOffset = Int64.(offset * (of_int sectorsPerBlock)) in
      let sector_size = sectorsPerBlock * info.BlockDevice.sector_size - sizeOfAddressInBytes in
      (* should in fact save log size_sectors space for the stash *)
      let height = calculateHeight desiredSizeInSectors info sectorsPerBlock sector_size bucketSize blockOffset in
      let numLeaves = Int64.(pow 2L (of_int height)) in
      let size_sectors = Int64.(bucketSize * (2L * numLeaves - 1L)) in
      let stash = Stash.create () in
      PositionMap.create ~desiredSizeInSectors:size_sectors ~bucketSize ~desiredBlockSize ~offset:Int64.(blockOffset + size_sectors) blockDevice >>= fun positionMap ->
      let t = {
          info = { read_write; sector_size; size_sectors } ;
          structuralInfo = { height ; numLeaves ; sectorsPerBlock } ;
          bucketSize ; desiredBlockSize ; offset = blockOffset ;
          stash ; positionMap ; blockDevice
        }
      in
      return (`Ok t)

  let create ?(desiredSizeInSectors = 0L) ?(bucketSize = 4L) ?(desiredBlockSize = 0x2000) ?(offset = 0L) blockDevice =
    Random.self_init ();
    lwt info = BlockDevice.get_info blockDevice in
      if info.BlockDevice.sector_size > sizeOfAddressInBytes
      then (
        let desiredBlockSizeToUse =
          if info.BlockDevice.sector_size > desiredBlockSize
          then info.BlockDevice.sector_size
          else desiredBlockSize
        in
        createInstanceOfType desiredSizeInSectors bucketSize desiredBlockSizeToUse offset blockDevice >>= fun t ->
        initialise t >>= fun () ->
        return (`Ok t)
      ) else return (`Error (`Unknown "Sector size too small to fit address"))

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

  let rec write t startAddress = function
    | [] -> return (`Ok ())
    | buffer :: buffers ->
       if startAddress >= t.info.size_sectors
       then return (`Error (`Unknown "End of file"))
       else
         writeBuffer t startAddress buffer >>= fun () ->
         let startAddressOfNextBuffer = Int64.(startAddress + (of_int buffer.Cstruct.len / (of_int t.info.sector_size))) in
         write t startAddressOfNextBuffer buffers

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

  let rec read t startAddress = function
    | [] -> return (`Ok ())
    | buffer :: buffers ->
       if startAddress >= t.info.size_sectors
       then return (`Error (`Unknown "End of file"))
       else
         readBuffer t startAddress buffer >>= fun () ->
         read t Int64.(startAddress + (of_int buffer.Cstruct.len / (of_int t.info.sector_size))) buffers

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
