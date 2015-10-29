open Bigarray

type t = (int64, int64_elt, c_layout) Array2.t

let max_32 = 4294967295L

let indices x =
  let x1 = Int64.(to_int @@ shift_right x 32) in
  let x2 = Int64.(to_int @@ logand x max_32) in
  (x1,x2)

let create x =
  let (x1,x2) = indices x in
  let x1' = x1 + 1 in
  let x2' = match x1' with
    | 1 -> x2
    | x -> Int64.to_int max_32
  in
  let empty = Array2.create Int64 c_layout x1' x2' in
  let bound = Int64.(div (add 1L @@ div x 4L) 2L) in
  for i = 0 to x1 - 1 do
    for j = 0 to Array2.dim2 empty - 1 do
      empty.{i,j} <- Random.int64 bound;
    done
  done;
  empty

let length t =
  let dim1 = Int64.of_int @@ Array2.dim1 t in
  let dim2 = Int64.of_int @@ Array2.dim2 t in
  Int64.mul dim1 dim2

let get t x =
  let (x1,x2) = indices x in
  t.{x1,x2}

let set t x y =
  let (x1,x2) = indices x in
  t.{x1,x2} <- y
