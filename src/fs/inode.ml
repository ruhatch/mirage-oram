(**

Address: uint64

Because int64 is actually signed, we use the sign bit to denote type

Negative values are indirect pointers and positive are direct pointers

*)

type t = Cstruct.t

type ptrType = Direct of int64 | Indirect of int64

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
  Cstruct.BE.set_uint64 t (i * 8) p'

let addPtrs t ps = t
  (* Take the list of pointers and add them to end of Cstruct - validation should be done at FS level *)

let prunePtrs t l =
  (* Change this to return the list recursively *)
  for i = l to length t - 1 do
    setPtr t i (Direct 0L)
  done
