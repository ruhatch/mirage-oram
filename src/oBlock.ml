open Core_kernel.Std

type t = int option * Bytes.t

let t_of_sexp = Tuple2.t_of_sexp (Option.t_of_sexp Int.t_of_sexp) String.t_of_sexp

let sexp_of_t = Tuple2.sexp_of_t (Option.sexp_of_t Int.sexp_of_t) String.sexp_of_t

let compare = Tuple2.compare ~cmp1:(Option.compare ~cmp:Int.compare) ~cmp2:String.compare

let hash = Hashtbl.hash

let empty = (None, Bytes.create 1)

let to_string (a,b) =
  match a with
    | Some x -> sprintf "%d : %s" x (BytesLabels.to_string (BytesLabels.escaped b))
    | None -> "DUMMY"
