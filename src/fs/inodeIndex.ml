type t = Cstruct.t

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
  in loop 0
