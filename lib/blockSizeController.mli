module Make (BlockDevice : V1_LWT.BLOCK) : sig

  include V1_LWT.BLOCK

  val connect : ?desiredBlockSize:int -> BlockDevice.t -> [`Error of error | `Ok of t] io

end
