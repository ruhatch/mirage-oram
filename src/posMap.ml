open Bigarray

type t = (int64, int64_elt, c_layout) Array2.t

let max_32 = 4294967296L

let create x =
  let x1 = Int64.(to_int @@ shift_right x 32) in
  let empty = Array2.create Int64 c_layout x1 @@ Int64.to_int max_32 in
  Array2.fill empty 0L;
  empty

let get t x =
  let x1 = Int64.(to_int @@ shift_right x 32) in
  let x2 = Int64.(to_int @@ logand x max_32) in
  t.{x1,x2}

let set t x y =
  let x1 = Int64.(to_int @@ shift_right x 32) in
  let x2 = Int64.(to_int @@ logand x max_32) in
  t.{x1,x2} <- y
