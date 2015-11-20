module type NODE = sig

  type t

  type pointer

  type key

  type value

  val create : int -> t

  val noKeys : t -> int

  val setNoKeys : t -> int -> unit

  val minDegree : t -> int

  val pageSize : t -> int

  val leaf : t -> bool

  val setLeaf : t -> bool -> unit

  val getChild : t -> int -> pointer

  val setChild : t -> int -> pointer -> unit

  val getKey : t -> int -> key

  val setKey : t -> int -> key -> unit

  val getValue : t -> int -> value

  val setValue : t -> int -> value -> unit

end

module Node = struct

  type t = Cstruct.t

  type pointer = int64

  type key = int

  type value = int64

  let create length =
    let minDegree = (length + 2) / 36 in
    let node = Cstruct.create (length * 8) in
    Cstruct.BE.set_uint16 node 0 0;
    Cstruct.BE.set_uint16 node 2 minDegree;
    Cstruct.BE.set_uint16 node 4 length;
    node

  let noKeys t =
    Cstruct.BE.get_uint16 t 0

  let setNoKeys t n =
    Cstruct.BE.set_uint16 t 0 n

  let minDegree t =
    Cstruct.BE.get_uint16 t 2

  let pageSize t =
    Cstruct.BE.get_uint16 t 4

  let leaf t =
    match Cstruct.BE.get_uint16 t 6 with
      | 0 -> false
      | 1 -> true

  let setLeaf t b =
    if b
    then Cstruct.BE.set_uint16 t 6 1
    else Cstruct.BE.set_uint16 t 6 0

  (* Add some bounds checking here (children from 1 to n+1) *)

  let getChild t i =
    Cstruct.BE.get_uint64 t (18 * i - 10)

  let setChild t i p =
    Cstruct.BE.set_uint64 t (18 * i - 10) p

  let getKey t i =
    Cstruct.BE.get_uint16 t (18 * i - 2)

  let setKey t i p =
    Cstruct.BE.set_uint16 t (18 * i - 2) p

  let getValue t i =
    Cstruct.BE.get_uint64 t (18 * i)

  let setValue t i p =
    Cstruct.BE.set_uint64 t (18 * i) p

end
