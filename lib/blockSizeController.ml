module Make (BlockDevice : V1_LWT.BLOCK) = struct

  type blockDevice = BlockDevice.t

  type error = BlockDevice.error

  type info = {
    read_write: bool;
    sector_size: int;
    size_sectors: int64;
  }

  type 'a io = 'a BlockDevice.io

  type page_aligned_buffer = BlockDevice.page_aligned_buffer

  type id = BlockDevice.id

  type t = {
    blockDevice : blockDevice;
    info : info;
    sectorsPerBlock : int;
  }

  let get_info { info } = Lwt.return info

  let disconnect { blockDevice } =
    BlockDevice.disconnect blockDevice

  let connect ?(desiredBlockSize = 0x100000) blockDevice =
    let%lwt info = BlockDevice.get_info blockDevice in
    let sectorsPerBlock = (desiredBlockSize - 1) / info.BlockDevice.sector_size + 1 in
    let read_write = info.BlockDevice.read_write in
    let sector_size = sectorsPerBlock * info.BlockDevice.sector_size in
    let size_sectors = Int64.(div info.BlockDevice.size_sectors (of_int sectorsPerBlock)) in
    Lwt.return (`Ok { blockDevice ; info = { read_write ; sector_size ; size_sectors } ; sectorsPerBlock })

  let write { blockDevice ; sectorsPerBlock } startAddress buffers =
    BlockDevice.write blockDevice Int64.(mul startAddress (of_int sectorsPerBlock)) buffers

  let read { blockDevice ; sectorsPerBlock } startAddress buffers =
    BlockDevice.read blockDevice Int64.(mul startAddress (of_int sectorsPerBlock)) buffers

end
