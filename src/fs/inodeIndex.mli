module Allocator : BTree.ALLOCATOR with type t = FreeMap.t and type pointer = int64

module Store (BlockDevice : V1_LWT.BLOCK) : BTree.STORE

module Make (BlockDevice : V1_LWT.BLOCK)
  : BTree.S with type pointer = Allocator.pointer
            and type allocator = Allocator.t
            and type store = BlockDevice.t
            and type error = BlockDevice.error
            and type 'a io = 'a Lwt.t
            and type node = Node.Node.t
            and type key = Node.Node.key
            and type value = Node.Node.value
