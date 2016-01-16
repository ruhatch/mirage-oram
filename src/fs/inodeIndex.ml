module Allocator = struct

  include FreeMap

  type pointer = int64

end

module Store (BlockDevice : V1_LWT.BLOCK) = struct

  type pointer = int64

  include BlockDevice

  let bind = Lwt.bind

  let return = Lwt.return

end

module Make (BlockDevice : V1_LWT.BLOCK) = struct

  include BTree.Make(Allocator)(Store(BlockDevice))(Node.Node)

end
