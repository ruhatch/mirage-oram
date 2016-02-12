type t = Bitarray.t

let get = Bitarray.get

let set = Bitarray.set

let create size_sectors sector_size =
  let result = Bitarray.create ~b:true (size_sectors * sector_size * 8) in
  for i = 0 to size_sectors do
    set result i false;
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
  in loop 1 n

let free t ps =
  let rec loop = function
    | [] -> ()
    | (p::ps) ->
      set t (Int64.to_int p) true;
      loop ps
  in loop ps
