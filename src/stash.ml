open Core_kernel.Std

include Hash_set.Make(OBlock)

let find_index t x = Hash_set.find t ~f:(fun (x',_) -> x' = x)

let length t = Hash_set.length t

let add t b =
  match b with
    | (-1L, _) -> ()
    | (x, _) -> Hash_set.add t b
