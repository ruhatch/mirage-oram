open Core_kernel.Std

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

module Make (BlockDevice : V1_LWT.BLOCK) = struct

  module FileSystem = Fs.Make(BlockDevice)

  type fileSystem = FileSystem.t
  type blockDevice = BlockDevice.t
  type error = BlockDevice.error
  type 'a io = 'a BlockDevice.io

  type t = {
    index : InvertedIndex.t;
    fileSystem : fileSystem;
    blockSize : int
  }

  let ( >>= ) x f = Lwt.bind x @@ function
    | `Error e -> Lwt.return (`Error e)
    | `Ok x -> f x

  let create blockDevice =
    Lwt.bind (BlockDevice.get_info blockDevice) @@ fun info ->
    let blockSize = info.BlockDevice.sector_size in
    let index = InvertedIndex.create () in
    FileSystem.initialise blockDevice >>= fun fileSystem ->
    Lwt.return (`Ok { index ; fileSystem ; blockSize })

  let flushIndex t =
    let indexFile = InvertedIndex.toCstruct t.index t.blockSize in
    FileSystem.writeFile t.fileSystem "files.index" indexFile

  let writeFile t name contents =
    Printf.printf "(0, %d)\n" (Time_ns.to_int_ns_since_epoch (Time_ns.now ()));
    FileSystem.writeFile t.fileSystem name contents >>= fun () ->
    InvertedIndex.indexFile t.index name contents;
    flushIndex t

  let readFile t name =
    Printf.printf "(0, %d)\n" (Time_ns.to_int_ns_since_epoch (Time_ns.now ()));
    FileSystem.readFile t.fileSystem name

  let search t query =
    Lwt.return (`Ok (InvertedIndex.fileNamesForQuery t.index query))

end
