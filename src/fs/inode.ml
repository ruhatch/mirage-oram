(**

Using direct pointers only for the convenience

Now layout inode as

| noPtrs l | pointer1 | ... | pointer n |

*)

type t = Cstruct.t

let create length =
  let t = Cstruct.create (length * 8) in
  Cstruct.BE.set_uint16 t 0 0;
  t

let noPtrs t =
  Cstruct.BE.get_uint16 t 0

let incrPtrs t =
  Cstruct.BE.set_uint16 t 0 (noPtrs t + 1)

let decrPtrs t =
  Cstruct.BE.set_uint16 t 0 (noPtrs t + 1)

let getPtr t i =
  Cstruct.BE.get_uint64 t ((i + 1) * 8)

let setPtr t i p =
  Cstruct.BE.set_uint64 t ((i + 1) * 8) p

let addPtrs t ps =
  let rec loop n = function
    | [] -> ()
    | (x::xs) ->
      setPtr t n x;
      incrPtrs t;
      loop (n + 1) xs
  in loop (noPtrs t + 1) ps

let prunePtrs t l =
  let rec loop n =
    if n = l
      then []
      else ( decrPtrs t; getPtr t n :: loop (n - 1) )
  in loop (noPtrs t)
