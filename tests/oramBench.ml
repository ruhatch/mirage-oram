open Core.Std
open Core_bench
open Testable

let oram = match Lwt_main.run (newORAM ()) with
  | `Ok oram -> oram
  | `Error _ -> failwith "Couldn't connect to ORAM"

let oram1 = match Lwt_main.run (newORAM ~fileName:"disk1.img" ()) with
  | `Ok oram -> oram
  | `Error _ -> failwith "Couldn't connect to larger ORAM"

let info = Lwt_main.run (O.get_info oram)

let info1 = Lwt_main.run (O.get_info oram1)

let data = Cstruct.create info.O.sector_size

let test1 =
  Bench.Test.create ~name:"Create ORAM" (fun () -> Lwt_main.run (newORAM ()))

let test2 =
  Bench.Test.create ~name:"Calculate bucket address"
                    (fun () -> O.calculateAddressOfBucket oram 5L 10)

let test3 =
  Bench.Test.create ~name:"Access block"
                    (fun () ->
                      O.access oram O.Write 0L (Some data))

let test4 =
  Bench.Test.create ~name:"Write block"
                    (fun () ->
                      O.write oram 0L [data])

let test5 =
  Bench.Test.create ~name:"Write block big ORAM"
                    (fun () ->
                      O.write oram1 0L [data])


let () =
  Printf.printf "ORAM1 has info:\n\tsize_sectors: %Ld\n\tsector_size:%d\n" info1.O.size_sectors info1.O.sector_size;
  Command.run (Bench.make_command [
                   test2;
                   test3;
                   test4;
                   test5
              ])
