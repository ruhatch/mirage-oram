(**

Using Core's Bitarray for now, but consider using Cstruct of some kind to ease writing to disk

*)

open Core_extended

type t = Bitarray.t

let get = Bitarray.get

let set = Bitarray.set

let create length =
  let result = Bitarray.create length in
  for i = 3 to length - 1 do
    set result i true
  done;
  result

let alloc t n =
  let rec loop pos = function
    | 0 -> []
    | n ->
      if get t pos
      then (
        set t pos false;
        (Int64.of_int pos) :: (loop (pos + 1) (n - 1))
      ) else loop (pos + 1) n
  in loop 3 n

let free t ps =
  let rec loop = function
    | [] -> ()
    | (p::ps) ->
      set t (Int64.to_int p) true;
      loop ps
  in loop ps
