open V1_LWT
open Printf

module Make (B: BLOCK) = struct

  include B

  let stash = Stash.create ()

  let write = Stash.add stash (Some 1, "test"); B.write (* Add thing to stash and then drop through to B's write *)

  let read = Printf.printf "Read out: %s" (Stash.find_index stash (Some 1)); B.read (* Find thing in stash and then drop through to B's read *)

end
