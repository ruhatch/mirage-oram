open PosMapIntf

module type ORAM = sig
  
  type t
         
  include V1_LWT.BLOCK
          with type t := t
          with type id = string

  include PosMap
          with type t := t
                     and type error := error

  (* Don't expose the initialise method *)
  (*val initialise : BlockDevice.t -> [`Ok of unit | `Error of error] Lwt.t*)
  (*val connect : BlockDevice.t -> [`Ok of t | `Error of error] Lwt.t*)
  val fakeReconnect : t -> block -> [`Ok of t | `Error of error] Lwt.t
  val disconnect : t -> unit Lwt.t
  val connect : block -> [`Ok of t | `Error  of error] Lwt.t

  (* Lower level functions - exposed for testing purposes *)

  val floor_log : int64 -> int

  type structuralInfo = {
      height : int;
      numLeaves : int64;
      sectorsPerBlock : int;
    }

  val getStructuralInfo : t -> structuralInfo Lwt.t

  val createPageAlignedBuffer : int -> Cstruct.t

  type bucket = OBlock.t list

  val calculateAddressOfBucket : t -> int64 -> int -> int64

  val writeBucket : t -> int64 -> bucket -> [`Ok of unit | `Error of error] Lwt.t

  val writePathToLeaf : t -> int64 -> bucket list -> [`Ok of unit | `Error of error] Lwt.t

  val readBucket : t -> int64 -> [`Ok of bucket | `Error of error] Lwt.t

  val readPathToLeaf : t -> int64 -> [`Ok of bucket list | `Error of error] Lwt.t

  type op = Read | Write

  val access : t -> op -> int64 -> Cstruct.t option -> [`Ok of Cstruct.t | `Error of error] Lwt.t

end

module Make (MakePositionMap : PosMapF) (BlockDevice : V1_LWT.BLOCK) : ORAM
       with type block = BlockDevice.t

module Builder (BlockDevice : V1_LWT.BLOCK) : sig

  val buildORAM : ?recursive:bool -> ?desiredSizeInSectors:int64 -> ?bucketSize:int64 -> ?desiredBlockSize:int -> BlockDevice.t -> [`Ok of (module ORAM with type block = BlockDevice.t) | `Error of BlockDevice.error] Lwt.t
                                                                                                                                     
end
