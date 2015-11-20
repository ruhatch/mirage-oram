(*type t = Cstruct.t

let create length =
  Cstruct.create (length * 8)

let length t =
  Cstruct.len t / 8

let get t i =
  Cstruct.BE.get_uint64 t (i * 8)

let set t i p =
  Cstruct.BE.set_uint64 t (i * 8) p

let nextFree t =
  let rec loop pos =
    if pos >= length t
    then failwith "No available inodes"
    else match get t pos with
      | 0L -> pos
      | _ -> loop (pos + 1)
  in loop 0*)

open V1_LWT

module A = struct

  include FreeMap

  type pointer = int64

end

module S (B : BLOCK) = struct

  type pointer = int64

  include B

  let bind = Lwt.bind

  let return = Lwt.return

end

module Make (B : BLOCK) = struct

  include BTree.Make(A)(S(B))(Node.Node)

end
