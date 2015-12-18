open Core_kernel.Std

type t = Int64.t * Cstruct.t

let t_of_sexp = Tuple2.t_of_sexp Int64.t_of_sexp Cstruct.t_of_sexp

let sexp_of_t = Tuple2.sexp_of_t Int64.sexp_of_t Cstruct.sexp_of_t

let compare = Tuple2.compare ~cmp1:Int64.compare ~cmp2:Cstruct.compare

let hash = Hashtbl.hash

let dummy x = (-1L, Cstruct.create x)

(*let to_string (a,b) =
  match a with
    | Some x -> sprintf "%d : %s" x (BytesLabels.to_string (BytesLabels.escaped b))
    | None -> "DUMMY"*)

(*let to_string (a,b) =
  let buf = String.create 8 in
  Binary_packing.pack_signed_64_little_endian ~buf ~pos:0 a;
  String.concat [buf; b]

let of_string buf =
  let a = Binary_packing.unpack_signed_64_little_endian ~buf ~pos:0 in
  let b = String.sub buf 8 (String.length buf - 8) in
  (a,b)*)

let to_cstruct (a,b) =
  let result = Cstruct.create (8 + Cstruct.len b) in
  Cstruct.BE.set_uint64 result 0 a;
  Cstruct.blit b 0 result 8 (Cstruct.len b);
  result

let of_cstruct buf =
  let a = Cstruct.BE.get_uint64 buf 0 in
  let b = Cstruct.sub buf 8 (Cstruct.len buf - 8) in
  (a,b)
