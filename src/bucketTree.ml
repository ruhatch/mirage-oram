open Core_kernel.Std

module Bucket = struct
  type t = OBlock.t * OBlock.t * OBlock.t
  let empty = (OBlock.empty, OBlock.empty, OBlock.empty)
  let to_string (b1, b2, b3) = sprintf "[%s, %s, %s]" (OBlock.to_string b1) (OBlock.to_string b2) (OBlock.to_string b3)
end

type tree = | Leaf of Bucket.t
            | Node of int * Bucket.t * tree * tree

type t = tree ref

let log2 x = log10 x /. (log10 2.)

let empty x =
  let height = Float.(to_int (round_up (log2 (round_up (Int64.to_float x /. 3.) +. 1.)))) - 1 in
  let rec empty' = function
    | 0 -> Leaf (Bucket.empty)
    | x -> Node (x, Bucket.empty, empty' (x - 1), empty' (x - 1))
  in
  ref (empty' height)


let to_string t =
  let rec to_string' lvl = function
      | Leaf b -> sprintf "%s%s\n\n"
                    (String.make (lvl * 2) ' ')
                    (Bucket.to_string b)
      | Node (_, b, l, r) -> sprintf "%s%s\n\n%s%s"
                               (String.make (lvl * 2) ' ')
                               (Bucket.to_string b)
                               (to_string' (lvl + 1) l)
                               (to_string' (lvl + 1) r)
  in
  to_string' 0 !t

let read_path_to_leaf t x =
  let rec read_path_to_leaf' t x =
    match t with
      | Leaf b -> [b]
      | Node (height, b, left, right) ->
        let divisor = Float.to_int (2. ** (float (height - 1))) in
        let sub = if x / divisor = 0 then left else right in
        let y = x mod divisor in
        b :: read_path_to_leaf' sub y
  in
  read_path_to_leaf' !t x

let write_path_to_leaf_exn t x bs =
  let rec write_path_to_leaf' t x bs =
    match bs with
      | [] -> failwith "Incorrect path length"
      | (b::bs) ->
        match t with
          | Leaf _ -> Leaf b
          | Node (height, _, left, right) ->
            let divisor = Float.to_int (2. ** (float (height - 1))) in
            let y = x mod divisor in
            if x / divisor = 0
            then Node (height, b, write_path_to_leaf' left y bs, right)
            else Node (height, b, left, write_path_to_leaf' right y bs)
  in
  t := write_path_to_leaf' !t x bs

let rec path_intercept x x' h = function
  | 0 -> true
  | l ->
    let divisor = Float.to_int (2. ** (float (h - l))) in
    if x / divisor = x' / divisor
    then path_intercept (x mod divisor) (x' mod divisor) h (l - 1)
    else false

(*let path_to_string = List.fold_left ~f:(fun acc b -> acc ^ (Bucket.to_string b)) ~init:""*)

let height t =
  match !t with
    | Leaf _ -> 0
    | Node (h, _, _, _) -> h

let leaves t =
  match !t with
    | Leaf _ -> 1
    | Node (h, _, _, _) -> Float.to_int (2. ** (Float.of_int h))

let size t =
  match !t with
    | Leaf _ -> 1
    | Node (h, _, _, _) -> Float.to_int (2. ** (Float.of_int (h + 1))) - 1

(*let () = let tree = empty 3 in
         printf "%s" (to_string tree);
         write_path_to_leaf_exn tree 3 [((Some 1, "test1"),(Some 1, "test1"),(Some 1, "test1")); ((Some 1, "test1"),(Some 1, "test1"),(Some 1, "test1")); ((Some 1, "test1"),(Some 1, "test1"),(Some 1, "test1")); ((Some 1, "test1"),(Some 1, "test1"),(Some 1, "test1"))];
         printf "%s" (to_string tree);
         printf "%s\n" (path_to_string (read_path_to_leaf tree 3))*)
