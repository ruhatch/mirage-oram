open V1_LWT

module Make :
  functor (P : PosMap.POSMAP) -> functor (B : BLOCK) ->
    sig

      include BLOCK
      with type id = string

      val create : ?size:int64 -> ?offset:int64 -> B.t -> [`Ok of t | `Error of B.error] Lwt.t
      val get : t -> int64 -> [`Ok of int64 | `Error of B.error] Lwt.t
      val set : t -> int64 -> int64 -> [`Ok of unit | `Error of B.error] Lwt.t
      val length : t -> int64

      val initialise : B.t -> [`Ok of unit | `Error of error] Lwt.t
      val connect : B.t -> [`Ok of t | `Error of error] Lwt.t

      (* Lower level functions - exposed for testing purposes *)

      val floor_log : int64 -> int

      type bucket = OBlock.t * OBlock.t * OBlock.t * OBlock.t

      val bucket_address : int64 -> int -> int64

      val write_bucket : t -> int64 -> bucket -> [`Ok of unit | `Error of error] Lwt.t

      val write_path : t -> int64 -> bucket list -> [`Ok of unit | `Error of error] Lwt.t

      val read_bucket : t -> int64 -> [`Ok of bucket | `Error of error] Lwt.t

      val read_path : t -> int64 -> [`Ok of bucket list | `Error of error] Lwt.t

    end
