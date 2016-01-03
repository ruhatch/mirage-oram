open V1_LWT
open PosMapIntf

module Make :
  functor (P : PosMapF) -> functor (B : BLOCK) ->
    sig

      type t

      include BLOCK
      with type t := t
      with type id = string

      include PosMap
      with type t := t
      and type block := B.t
      and type error := B.error

      val initialise : B.t -> [`Ok of unit | `Error of error] Lwt.t
      (*val connect : B.t -> [`Ok of t | `Error of error] Lwt.t*)

      (* Lower level functions - exposed for testing purposes *)

      val floor_log : int64 -> int

      type bucket = OBlock.t list

      val bucket_address : t -> int64 -> int -> int64

      val write_bucket : t -> int64 -> bucket -> [`Ok of unit | `Error of error] Lwt.t

      val write_path : t -> int64 -> bucket list -> [`Ok of unit | `Error of error] Lwt.t

      val read_bucket : t -> int64 -> [`Ok of bucket | `Error of error] Lwt.t

      val read_path : t -> int64 -> [`Ok of bucket list | `Error of error] Lwt.t

    end
