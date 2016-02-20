open Lwt
open Alcotest

let int64 =
  let module M = struct
    type t = int64
    let pp fmt t = Format.fprintf fmt "%Ld" t
    let equal = (=)
  end in
  (module M: TESTABLE with type t = M.t)

let tuple3 (type a) (type b) (type c) (a:a testable) (b:b testable) (c:c testable): (a * b * c) testable =
  let module A = (val a) in
  let module B = (val b) in
  let module C = (val c) in
  (module struct
    type t = a * b * c
    let equal (a1, b1, c1) (a2, b2, c2) = A.equal a1 a2 && B.equal b1 b2 && C.equal c1 c2
    let pp ppf (a, b, c) = A.pp ppf a; Format.pp_print_cut ppf (); B.pp ppf b; Format.pp_print_cut ppf (); C.pp ppf c
  end)

let cstruct =
  let module M = struct
    type t = Cstruct.t
    let pp fmt t = Format.pp_print_string fmt (Cstruct.to_string t)
    let equal = Cstruct.equal
  end in
  (module M: TESTABLE with type t = M.t)

let error =
  let module M = struct
    type t = [
      | `Unknown of string
      | `Unimplemented
      | `Is_read_only
      | `Disconnected
    ]
    let pp fmt t = match t with
      | `Unknown s -> Format.fprintf fmt "Unknown: %s" s
      | `Unimplemented -> Format.pp_print_string fmt "Unimplemented"
      | `Is_read_only -> Format.pp_print_string fmt "Is read only"
      | `Disconnected -> Format.pp_print_string fmt "Disconnected"
    let equal = (=)
  end in
  (module M: TESTABLE with type t = M.t)

let result (type a) error (type b) ok =
  let (module Error: Alcotest.TESTABLE with type t = a) = error in
  let (module Ok: Alcotest.TESTABLE with type t = b) = ok in
  let module M = struct
    type t = [ `Error of a | `Ok of b ]
    let pp fmt t = match t with
      | `Ok x -> Format.fprintf fmt "Ok @[(%a)@]" Ok.pp x
      | `Error x -> Format.fprintf fmt "Error: @[%a@]" Error.pp x
    let equal x y = match x, y with
      | `Ok x, `Ok y -> Ok.equal x y
      | `Error x, `Error y   -> Error.equal x y
      | _ -> false
  end in
  (module M: TESTABLE with type t = M.t)

let lwt_t (type a) elt =
  let (module Elt: TESTABLE with type t = a) = elt in
  let module M = struct
    type t = unit -> a Lwt.t
    let pp fmt t = match Lwt_main.run (t ()) with
      | x -> Format.fprintf fmt "@[%a@]" Elt.pp x
    let equal x y =
      let x' = Lwt_main.run (x ()) in
      let y' = Lwt_main.run (y ()) in
      Elt.equal x' y'
  end in
  (module M: TESTABLE with type t = M.t)

let oblock = pair int64 cstruct

let bucket = list oblock

(* Helper modules and functions for testing *)

(* module O = Oram.Make(Oram.Make(Oram.Make(PosMap.InMemory)))(Block) *)

(* module O = Oram.Make(Oram.Make(PosMap.InMemory))(Block) *)

module O = Oram.Make(PosMap.InMemory)(Block)

(* module O = Block *)

module F = Fs.Make(O)

let ( >>= ) x f = x >>= function
  | `Error e -> return (`Error e)
  | `Ok x -> f x

let readWholeFile name =
  let file = Unix.openfile name [Unix.O_RDWR] 0o664 in
  let length = (Unix.fstat file).Unix.st_size in
  let buff = Bytes.create length in
  let rec loop off = function
    | 0 -> buff
    | n ->
      let read = Unix.read file buff off n in
      loop (off + read) (n - read)
  in loop 0 length

let writeWholeFile name contents =
  let file = Unix.openfile name [Unix.O_RDWR; Unix.O_CREAT] 0o664 in
  let length = Cstruct.len contents in
  let buff = Bytes.create length in
  Cstruct.blit_to_string contents 0 buff 0 length;
  let rec loop off = function
    | 0 -> ()
    | n ->
      let written = Unix.write file buff off n in
      loop (off + written) (n - written)
  in loop 0 length

let newFile (oram : O.t) contents =
  let%lwt info = O.get_info oram in
  let sectorLength = (String.length contents - 1) / info.O.sector_size + 1 in
  let file = Cstruct.create (sectorLength * info.O.sector_size) in
  for i = 0 to Cstruct.len file - 1 do
    Cstruct.set_uint8 file i 0
  done;
  Cstruct.blit_from_string contents 0 file 0 (String.length contents);
  return (`Ok file)

let newORAM ?(fileName = "disk.img") ?(desiredSizeInSectors = 0L) () =
  Block.connect fileName >>= fun bd ->
  O.create ~desiredSizeInSectors bd

let newFs () =
  newORAM () >>= fun bd ->
  F.initialise bd

module SearchClient = SearchClient.Make(O)

let newSearchClient () =
  newORAM () >>= fun blockDevice ->
  SearchClient.create blockDevice
