open Core_kernel.Std

include Hash_set.Make(OBlock)

let find_index t x =
  let b = Hash_set.find t ~f:(fun (x',_) -> x' = x) in
  match b with
    | Some (_,d) -> d
    | None -> ""

let add t b =
  match b with
    | (Some x, _) -> Hash_set.add t b
    | (None, _) -> ()
