let int64 =
  let module M = struct
    type t = int64
    let pp fmt t = Format.fprintf fmt "%Ld" t
    let equal = (=)
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)

let oblock = Alcotest.(pair int64 string)
