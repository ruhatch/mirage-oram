open V1_LWT
open PosMapIntf

module InMemory : functor (B : BLOCK) -> sig

  include PosMap
  with type block = B.t
  and type error = B.error

  val indices : int64 -> int * int * int

end
