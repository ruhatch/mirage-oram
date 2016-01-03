open V1_LWT
open Lwt

module type PosMap = sig

  type t
  type block
  type error
  val create : ?size:int64 -> ?offset:int64 -> ?bucketSize:int64 -> block -> [`Ok of t | `Error of error] Lwt.t
  val get : t -> int64 -> [`Ok of int64 | `Error of error] Lwt.t
  val set : t -> int64 -> int64 -> [`Ok of unit | `Error of error] Lwt.t
  val length : t -> int64

end

module type PosMapF = functor (B : BLOCK) -> sig

  include PosMap with type block := B.t and type error := B.error

end
