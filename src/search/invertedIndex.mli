type t

val create : unit -> t

val updatePosting : t -> string -> string -> unit

val indexFile : t -> string -> Cstruct.t -> unit

val fileNamesForQuery : t -> string -> string list

val toCstruct : t -> int -> Cstruct.t

val ofCstruct : Cstruct.t -> t
