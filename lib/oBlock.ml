open Core_kernel.Std

type t = Int64.t * Cstruct.t

let t_of_sexp = Tuple2.t_of_sexp Int64.t_of_sexp Cstruct.t_of_sexp

let sexp_of_t = Tuple2.sexp_of_t Int64.sexp_of_t Cstruct.sexp_of_t

let compare = Tuple2.compare ~cmp1:Int64.compare ~cmp2:Cstruct.compare

let hash = Hashtbl.hash

let dummy x = (-1L, Cstruct.create x)

let to_cstruct (a, b) =
  let bytes = Cstruct.len b + 8 in
  let sizeInPages = Io_page.round_to_page_size bytes / Io_page.page_size in
  let result = Cstruct.sub (Io_page.get_buf ~n:sizeInPages ()) 0 bytes in
  Cstruct.BE.set_uint64 result 0 a;
  Cstruct.blit b 0 result 8 (Cstruct.len b);
  result

let of_cstruct buf =
  let a = Cstruct.BE.get_uint64 buf 0 in
  let b = Cstruct.sub buf 8 (Cstruct.len buf - 8) in
  (a, b)
