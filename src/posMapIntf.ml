open V1_LWT
open Lwt

module type POSMAP = functor (B : BLOCK) -> sig

  type t
  val create : ?size:int64 -> ?offset:int64 -> B.t -> [`Ok of t | `Error of B.error] Lwt.t
  val get : t -> int64 -> [`Ok of int64 | `Error of B.error] Lwt.t
  val set : t -> int64 -> int64 -> [`Ok of unit | `Error of B.error] Lwt.t
  val length : t -> int64

end
