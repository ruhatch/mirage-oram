(**

  Simple non-persistent B-Trees

  Pass in block size on creation

  This determines t (minimum degree) as t = (B + 10) / 36

  -6 off the front for pointers and floor it, so t = |_ (B + 4) / 36 _|

  Thus for 512B sectors, get minimum degree of 16 children

  Need to store t somewhere

  Root node is B-Tree and stores t as well as other things

  Each node is then a Cstruct laid out as

  -----------------------------------------------------------------------------------------------------------------
  | noKeys n | minDegree t | pageSize p | child 1 | key 1 | value 1 | child 2 | ... | key n | value n | child n+1 |
  -----------------------------------------------------------------------------------------------------------------

  Child is a pointer to another Node of the B-Tree (uint64)
  Key is an inode number based on the hash of the filename (uint16)
  Value is a pointer to the actual inode (uint64)

*)

module STORE = sig

  allocate

end

module Node (S : STORE) = struct

  type t = Cstruct.t

  let create length =
    let minDegree = (length + 4) / 36 in
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
    Cstruct.BE.get_unit16 t 4

  (* Add some bounds checking here (children from 1 to n+1) *)

  let getChild t i =
    Cstruct.BE.get_uint64 t (18 * i - 12)

  let setChild t i p =
    Cstruct.BE.set_uint64 t (18 * i - 12) p

  let getKey t i =
    Cstruct.BE.get_uint16 t (18 * i - 4)

  let setKey t i p =
    Cstruct.BE.set_uint16 t (18 * i - 4) p

  let getValue t i =
    Cstruct.BE.get_uint64 t (18 * i - 2)

  let setValue t i p =
    Cstruct.BE.set_uint64 t (18 * i - 2) p

end

let create pageSize = Node.create pageSize

let splitChild x i =
  let z = create (Node.pageSize x) in
  let y = x.getChild

let insert t k v =
  let minDegree = Node.minDegree t in
  if Node.noKeys t = (2 * minDegree + 1)
  then (
    let s = create (Node.pageSize t) in
    Node.setChild s 1 t;
    splitChild s 1;
    insertNonfull s k v
  ) else insertNonfull t k v
