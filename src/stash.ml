open Core_kernel.Std

type t = (int64, Cstruct.t) Hashtbl.t

let create () = Int64.Table.create ()

let length = Int64.Table.length

let add t ~address ~data =
  if not (address = -1L)
    then Int64.Table.set t ~key:address ~data

let find = Int64.Table.find

let remove = Int64.Table.remove

let to_alist = Int64.Table.to_alist
