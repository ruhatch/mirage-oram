open Core_kernel.Std

(*include Hash_set.Make(OBlock)

let find_index t x = Hash_set.find t ~f:(fun (x',_) -> x' = x)

let length t = Hash_set.length t

let add t b =
  match b with
    | (-1L, _) -> ()
    | (x, _) -> Hash_set.add t b*)

type t = (int64, Cstruct.t) Hashtbl.t

let create () = Int64.Table.create ()

let length = Int64.Table.length

let add t ~address ~data =
  if not (address = -1L)
    then Int64.Table.set t ~key:address ~data

let find = Int64.Table.find

let remove = Int64.Table.remove

let to_alist = Int64.Table.to_alist
