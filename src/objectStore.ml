open Lwt
open V1_LWT

module Make (B: BLOCK) = struct

  (* Layout of initialised object store

    0: Marker and metadata item for the index file (mainly for length of file)
    1: index file
    2: other files

  *)

  module StringHash =

    struct

      type t = int

      let equal = (=)

      let hash = Hashtbl.hash

    end

  module StringHashtbl = Hashtbl.Make(StringHash)

  (* Address and Number of blocks *)
  type metadata = (int64, int32)

  (* Need to choose a data structure for storing the metadata (file locations and length) *)
  let index = StringHashtbl.create 256

  type t = {
    index : metadata StringHashtbl.t;
    bd : B.t;
  }

  let init bd =
    (* Check if index file is inside block device *)
    lwt info = B.get_info bd in
    let buf = Cstruct.create info.sector_size in
    match_lwt read bd 0L [buf] with
      | `Ok () ->


    (* If not present, then create new index file and then store it *)

    (* If present, load it and set record field *)
    { index ; bd }

  let read t ~name =
    (* Calculate address for name and perform look up in bd *)
    let address = StringHashtbl.find t.index

  let write t ~name contents =
    (* Calculate address for name and perform right in bd *)
    let address =

  let delete t ~name =
    (* Look up address in metadata structure and then delete if exists *)

end
