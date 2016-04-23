
open Core.Std

module O = Oram.Make(PosMap.InMemory)(Block)

let ( >>= ) x f = Lwt.bind x @@ function
  | `Error e -> Lwt.return (`Error e)
  | `Ok x -> f x

let connectAndInitialiseORAMOfSize desiredSizeInSectors =
  match Lwt_main.run (
            Block.connect "diskSecurity.img" >>= fun bd ->
            O.create ~desiredSizeInSectors bd) with
  | `Ok oram -> oram
  | `Error _ -> failwith (Printf.sprintf "Failed to connect to oram with size %Ld" desiredSizeInSectors)

let connectToORAM desiredSizeInSectors =
  match Lwt_main.run (
            Block.connect "diskSecurity.img" >>= fun bd ->
            O.connect bd) with
  | `Ok oram -> oram
  | `Error _ -> failwith (Printf.sprintf "Failed to connect to oram with size %Ld" desiredSizeInSectors)

let desiredSizes minHeight maxHeight =
  let heights = List.range minHeight maxHeight in
  List.map ~f:(fun height -> Int64.of_int ((Int.pow 2 (height + 1) - 1) * 4)) heights

let performExperiment oram data desiredSizeInSectors iterations =
  let address = Random.int64 desiredSizeInSectors in
  let rec loopWrite = function
    | 0 -> Lwt.return (`Ok ())
    | n ->
      O.write oram address [data] >>= fun () ->
      loopRead (n - 1)
  and loopRead = function
  | 0 -> Lwt.return (`Ok ())
  | n ->
    O.read oram address [data] >>= fun () ->
    loopWrite (n - 1)
  in loopWrite iterations

let () =
  Command.basic
    ~summary:"Access a random block repeatedly, printing the access pattern"
    Command.Spec.(
    empty
    +> anon ("iterations" %: int)
    +> anon ("height" %: int)
    +> flag "i" no_arg ~doc:"Pass in this flag to initialise block devices before connecting")
    (fun iterations height shouldInitialise () ->
      List.iter (desiredSizes height (height + 1))
                ~f:(fun desiredSizeInSectors ->
                  let oram =
                    if shouldInitialise
                    then connectAndInitialiseORAMOfSize desiredSizeInSectors
                    else connectToORAM desiredSizeInSectors in
                  let info = Lwt_main.run (O.get_info oram) in
                  let data = Cstruct.create info.O.sector_size in
                  begin match Lwt_main.run (performExperiment oram data desiredSizeInSectors iterations) with
                  | `Ok () -> ()
                  | `Error _ -> failwith "Failed to perform experiment"
                  end))
  |> Command.run
