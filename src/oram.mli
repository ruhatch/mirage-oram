open PosMapIntf

module Make (P : PosMapF) (B : V1_LWT.BLOCK) : sig

  type t

  include V1_LWT.BLOCK
          with type t := t
          with type id = string

  include PosMap
          with type t := t
                     and type block := B.t
                                   and type error := B.error

  (* Don't expose the initialise method *)
  (*val initialise : B.t -> [`Ok of unit | `Error of error] Lwt.t*)
  (*val connect : B.t -> [`Ok of t | `Error of error] Lwt.t*)
  val fakeReconnect : t -> B.t -> [`Ok of t | `Error of error] Lwt.t
  val disconnect : t -> unit Lwt.t

  (* Lower level functions - exposed for testing purposes *)

  val floor_log : int64 -> int

  type structuralInfo = {
      height : int;
      numLeaves : int64;
      sectorsPerBlock : int;
    }

  val getStructuralInfo : t -> structuralInfo Lwt.t

  type bucket = OBlock.t list

  val calculateAddressOfBucket : t -> int64 -> int -> int64

  val writeBucket : t -> int64 -> bucket -> [`Ok of unit | `Error of error] Lwt.t

  val writePathToLeaf : t -> int64 -> bucket list -> [`Ok of unit | `Error of error] Lwt.t

  val readBucket : t -> int64 -> [`Ok of bucket | `Error of error] Lwt.t

  val readPathToLeaf : t -> int64 -> [`Ok of bucket list | `Error of error] Lwt.t

  type op = Read | Write

  val access : t -> op -> int64 -> Cstruct.t option -> [`Ok of Cstruct.t | `Error of error] Lwt.t

end
