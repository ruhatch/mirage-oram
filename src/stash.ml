open Core_kernel.Std

type t = Cstruct.t Int64.Table.t

let bin_size_cstruct cstruct =
  let string = Cstruct.to_string cstruct in
  bin_size_string string

let bin_write_cstruct buf ~pos cstruct =
  let string = Cstruct.to_string cstruct in
  bin_write_string buf ~pos string

let bin_read_cstruct buf ~pos_ref =
  let string = bin_read_string buf ~pos_ref in
  Cstruct.of_string string

let bin_size_t t =
  Int64.Table.bin_size_t bin_size_cstruct t

let bin_write_t buf ~pos t =
  Int64.Table.bin_write_t bin_write_cstruct buf ~pos t

let bin_read_t buf ~pos_ref =
  Int64.Table.bin_read_t bin_read_cstruct buf ~pos_ref

let create () = Int64.Table.create ()

let length = Int64.Table.length

let add t ~address ~data =
  if not (address = -1L)
    then Int64.Table.set t ~key:address ~data

let find = Int64.Table.find

let remove = Int64.Table.remove

let to_alist = Int64.Table.to_alist
