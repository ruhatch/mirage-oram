open Bigarray

type t = (int64, int64_elt, c_layout) Array2.t

let max_32 = 4294967296L

let create x =
  let x1 = Int64.(to_int @@ add (shift_right x 32) 1L) in
  let x2 = match x1 with
    | 1 -> Int64.(to_int @@ logand x (sub max_32 1L))
    | x -> Int64.to_int max_32
  in
  let empty = Array2.create Int64 c_layout x1 x2 in
  let bound = Int64.(div (add 1L @@ div x 4L) 2L) in
  for i = 0 to x1 - 1 do
    for j = 0 to Array2.dim2 empty - 1 do
      empty.{i,j} <- Random.int64 bound;
    done
  done;
  empty

let get t x =
  let x1 = Int64.(to_int @@ shift_right x 32) in
  (*Printf.printf "x1: %d\n" x1;*)
  let x2 = Int64.(to_int @@ logand x (sub max_32 1L)) in
  (*Printf.printf "x2: %d\n" x2;*)
  t.{x1,x2}

let set t x y =
  let x1 = Int64.(to_int @@ shift_right x 32) in
  let x2 = Int64.(to_int @@ logand x (sub max_32 1L)) in
  t.{x1,x2} <- y
