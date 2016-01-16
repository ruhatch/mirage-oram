(* Note: We're not currently interested in deletes, so no need for a FreeMap yet! *)

(* Also don't care about efficiency yet, so a Cstruct is fine for the map *)

type postings = Cstruct.t
type docIdMap = Cstruct.t

t = {
  postings : postings;
  docIdMap : docIdMap;
  mutable postingLength : int;
  mutable nextFreeSpace : int;
}

let nextFreeSpace t = t.nextFreeSpace

let lookupDocId t docId =
  let rec idAtIndex 

let addPostingsList postingsList =
  let
