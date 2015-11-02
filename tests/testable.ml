let int64 =
  let module M = struct
    type t = int64
    let pp fmt t = Format.fprintf fmt "%Ld" t
    let equal = (=)
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)

let lwt_t (type a) elt =
  let (module Elt: Alcotest.TESTABLE with type t = a) = elt in
  let module M = struct
    type t = unit -> a Lwt.t
    let pp fmt t = match Lwt_main.run (t ()) with
      | x -> Format.fprintf fmt "@[%a@]" Elt.pp x
    let equal x y =
      let x' = Lwt_main.run (x ()) in
      let y' = Lwt_main.run (y ()) in
      x' = y'
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)

let oblock = Alcotest.(pair int64 string)
