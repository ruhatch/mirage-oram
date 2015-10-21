open V1_LWT

module Make :
  functor (B : BLOCK) ->
    sig

      include BLOCK
      with type id = string

      val connect : B.t -> [`Ok of t | `Error of error] Lwt.t

    end
