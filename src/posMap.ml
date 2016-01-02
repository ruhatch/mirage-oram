open Bigarray
open V1_LWT
open Core_kernel.Std
open Lwt

module InMemory (B : BLOCK) = struct

  type t = (int64, int64_elt, c_layout) Array2.t

  let max_32 = 4294967295L

  let indices x =
    let x1 = Option.value Int64.(to_int @@ shift_right x 32) ~default:0 in
    let x2 = Option.value Int64.(to_int @@ bit_and x max_32) ~default:0 in
    (x1,x2)

  let floor_log x =
    let rec loop acc = function
      | 1L -> acc
      | x -> loop (acc + 1) Int64.(x / 2L)
    in loop 0 x

  let create ?(size=0L) ?(offset=0L) ?(bucketSize = 4L) bd =
    lwt info = B.get_info bd in
    let height =
      if size = 0L
        (* This shouldn't happen, but should check anyway *)
        then floor_log Int64.(info.B.size_sectors / bucketSize + 1L) - 1
        else floor_log Int64.(size / bucketSize + 1L) - 1
    in
    let bound = Int64.(pow 2L (of_int height)) in
    let x = Int64.(bucketSize * (2L * bound - 1L)) in
    Printf.printf "Created posmap of size %Ld\n" x;
    let (x1,x2) = indices x in
    let x1' = x1 + 1 in
    let x2' = match x1' with
      | 1 -> x2
      | x -> Option.value Int64.(to_int max_32) ~default:0
    in
    let empty = Array2.create Int64 c_layout x1' x2' in
    for i = 0 to x1' - 1 do
      for j = 0 to x2' - 1 do
        empty.{i,j} <- Random.int64 bound
      done
    done;
    return (`Ok empty)

  let length t =
    let dim1 = Int64.of_int @@ Array2.dim1 t in
    let dim2 = Int64.of_int @@ Array2.dim2 t in
    Int64.(dim1 * dim2)

  let get t x =
    let (x1,x2) = indices x in
    return (`Ok t.{x1,x2})

  let set t x y =
    let (x1,x2) = indices x in
    return (`Ok (t.{x1,x2} <- y))

end
