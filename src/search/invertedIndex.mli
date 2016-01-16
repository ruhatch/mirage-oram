type t

val create : unit -> t

val updatePosting : t -> string -> int -> unit

val indexFile : t -> Cstruct.t -> int -> unit

val find : t -> string -> int list

val toCstruct : t -> int -> Cstruct.t

val ofCstruct : Cstruct.t -> t
