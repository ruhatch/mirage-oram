open Mirage

let main = foreign "Unikernel.Main" (console @-> block @-> job)

let oramify = foreign "Oram.Make(PosMap.InMemory)" (block @-> block)

let img = match get_mode () with
  | `Xen -> oramify $ block_of_file "xvda1"
  | `Unix | `MacOSX -> oramify $ block_of_file "disk.img"

let () =
  add_to_ocamlfind_libraries ["mirage-oram"];
  register "oram-client" [main $ default_console $ img]
