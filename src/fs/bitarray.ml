(**

  Need to access using int64

  For now we will just use ints, but we will have to create an array of cstructs for full coverage

*)

type t = Cstruct.t

let create ?(b = false) n =
  let result = Cstruct.create ((n - 1) / 8 + 1) in
  begin match b with
    | false ->
      for i = 0 to Cstruct.len result - 1 do
        Cstruct.set_uint8 result i 0
      done
    | true ->
      for i = 0 to Cstruct.len result - 1 do
        Cstruct.set_uint8 result i 255
      done
  end;
  result

let get t i =
  let block = Cstruct.get_uint8 t (i / 8) in
  let j = 7 - i mod 8 in
  match (block lsr j) land 1 with
    | 0 -> false
    | 1 -> true
    | _ -> failwith "Neither true nor false..."

let set t i b =
  let block = Cstruct.get_uint8 t (i / 8) in
  let j = 7 - i mod 8 in
  let block' =
    if b
      then block lor (1 lsl j)
      else lnot ((lnot block) lor (1 lsl j))
  in
  Cstruct.set_uint8 t (i / 8) block'

let to_list t =
  let rec loop acc = function
    | 0 -> acc
    | n -> loop ((get t (n - 1)) :: acc) (n - 1)
  in
  loop [] (Cstruct.len t * 8)
