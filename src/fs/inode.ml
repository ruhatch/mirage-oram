(**

Address: uint64

Because int64 is actually signed, we use the sign bit to denote type

Negative values are indirect pointers and positive are direct pointers

*** Changing the above ***

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

(*type ptrType = Direct of int64 | Indirect of int64

let typeConv i =
  Int64.(add 1L @@ logxor (-1L) i)

let typeOfPtr i =
  if i > 0L
  then Direct i
  else Indirect (typeConv i)

let create length =
  Cstruct.create (length * 8)

let length t =
  Cstruct.len t / 8

let getPtr t i =
  typeOfPtr (Cstruct.BE.get_uint64 t (i * 8))

let setPtr t i p =
  let p' = match p with
    | Direct x -> x
    | Indirect x -> typeConv x
  in
  Cstruct.BE.set_uint64 t (i * 8) p'*)

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
