open Bigarray
open Core_kernel.Std
open Lwt
open Bin_prot.Std
open Bin_prot.Common

module InMemory (BlockDevice : V1_LWT.BLOCK) = struct

  type t = (int64, int64_elt, c_layout) Array3.t

  type block = BlockDevice.t

  type error = BlockDevice.error

  external buf_of_array3 : t -> buf = "%identity"

  let bin_size_t t =
    let len1 = Array3.dim1 t in
    let len2 = Array3.dim2 t in
    let len3 = Array3.dim3 t in
    let size = len1 * len2 * len3 * 8 in
    bin_size_int len1 + bin_size_int len2 + bin_size_int len3 + size

  let bin_write_t buf ~pos t =
    let len1 = Array3.dim1 t in
    let len2 = Array3.dim2 t in
    let len3 = Array3.dim3 t in
    let pos = bin_write_int buf ~pos len1 in
    let pos = bin_write_int buf ~pos len2 in
    let pos = bin_write_int buf ~pos len3 in
    let size = len1 * len2 * len3 * 8 in
    let next = pos + size in
    check_next buf next;
    unsafe_blit_buf ~src:(buf_of_array3 t) ~src_pos:0 ~dst:buf ~dst_pos:pos ~len:size;
    next

  let bin_writer_t = { Bin_prot.Type_class.write = bin_write_t ; size = bin_size_t }

  let bin_read_t buf ~pos_ref blockDevice =
    let len1 = (bin_read_int buf ~pos_ref) in
    let len2 = (bin_read_int buf ~pos_ref) in
    let len3 = (bin_read_int buf ~pos_ref) in
    let size = len1 * len2 * len3 * 8 in
    let pos = !pos_ref in
    let next = pos + size in
    check_next buf next;
    let t = Array3.create Int64 c_layout len1 len2 len3 in
    unsafe_blit_buf ~src:buf ~src_pos:pos ~dst:(buf_of_array3 t) ~dst_pos:0 ~len:size;
    pos_ref := next;
    t

  let diskSize _ = 0L

  let indices a =
    let x = Option.value Int64.(to_int @@ shift_right a 60) ~default:0 in
    let y = Option.value Int64.(to_int @@ bit_and (shift_right a 30) 0x3FFFFFFFL) ~default:0 in
    let z = Option.value Int64.(to_int @@ bit_and a 0x3FFFFFFFL) ~default:0 in
    (x, y, z)

  let floor_log x =
    let rec loop acc = function
      | 1L -> acc
      | x -> loop (acc + 1) Int64.(x / 2L)
    in loop 0 x

  let create ?(desiredSizeInSectors=0L) ?(bucketSize = 4L) ?(desiredBlockSize = 0x40000) ?(offset=0L) blockDevice =
    let height = floor_log Int64.(desiredSizeInSectors / bucketSize + 1L) - 1 in
    let bound = Int64.(pow 2L (of_int height)) in
    let a = Int64.(bucketSize * (2L * bound - 1L)) in
    let (x, y, z) = indices a in
    let x' = x + 1 in
    let y' = match x' with
      | 1 -> y + 1
      | x -> 0x3FFFFFFF
    in
    let z' = match y' with
      | 1 -> z
      | x -> 0x3FFFFFFF
    in
    let empty = Array3.create Int64 c_layout x' y' z' in
    for i = 0 to x' - 1 do
      for j = 0 to y' - 1 do
        for k = 0 to z' - 1 do
          empty.{i,j,k} <- Random.int64 bound
        done
      done
    done;
    return (`Ok empty)

  let length t =
    let dim1 = Int64.of_int @@ Array3.dim1 t in
    let dim2 = Int64.of_int @@ Array3.dim2 t in
    let dim3 = Int64.of_int @@ Array3.dim3 t in
    Int64.(dim1 * dim2 * dim3)

  let get t a =
    let (x, y, z) = indices a in
    return (`Ok t.{x,y,z})

  let set t a d =
    let (x, y, z) = indices a in
    return (`Ok (t.{x,y,z} <- d))

end
