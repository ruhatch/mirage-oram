open V1_LWT
open Printf
open Lwt

module Make (B: BLOCK) = struct

  include B

  let stash = Stash.create ()

  let number = ref 0

  let write a = Stash.add stash (Some !number, sprintf "test%d" !number); incr number; B.write a

  let read a = B.read a

end
