module type S = sig

  type fileSystem
  type blockDevice
  type error
  type 'a io

  type t = {
    index : InvertedIndex.t;
    fileSystem : fileSystem;
    blockSize : int
  }

  val create : blockDevice -> [`Error of error | `Ok of t] io

  val writeFile : t -> string -> Cstruct.t -> [`Error of error | `Ok of unit] io

  val readFile : t -> string -> [`Error of error | `Ok of Cstruct.t] io

  val search : t -> string -> [`Error of error | `Ok of string list] io

end

module Make (BlockDevice : V1_LWT.BLOCK)
  : S with type fileSystem = Fs.Make(BlockDevice).t
      and type blockDevice = BlockDevice.t
      and type error = BlockDevice.error
      and type 'a io = 'a BlockDevice.io
