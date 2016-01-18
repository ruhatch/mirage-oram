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

module Make (BlockDevice : V1_LWT.BLOCK)
  : S with type blockDevice = BlockDevice.t
      and type blockDeviceInfo = BlockDevice.info
      and type error = BlockDevice.error
