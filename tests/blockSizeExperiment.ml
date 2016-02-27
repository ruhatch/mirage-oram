open Core.Std

module O = Oram.Make(PosMap.InMemory)(Block)

let ( >>= ) x f = Lwt.bind x @@ function
  | `Error e -> Lwt.return (`Error e)
  | `Ok x -> f x

let connectAndInitialiseORAMOfSize desiredSizeInSectors desiredBlockSize =
  match Lwt_main.run (
            Block.connect (Printf.sprintf "disk%Ld.img" desiredSizeInSectors) >>= fun bd ->
            O.create ~desiredBlockSize bd) with
  | `Ok oram -> oram
  | `Error _ -> failwith (Printf.sprintf "Failed to connect to oram with size %Ld" desiredSizeInSectors)

let connectToORAM desiredSizeInSectors =
  match Lwt_main.run (
            Block.connect (Printf.sprintf "disk%Ld.img" desiredSizeInSectors) >>= fun bd ->
            O.connect bd) with
  | `Ok oram -> oram
  | `Error _ -> failwith (Printf.sprintf "Failed to connect to oram with size %Ld" desiredSizeInSectors)

let desiredSizes minHeight maxHeight =
  let heights = List.range minHeight maxHeight in
  List.map ~f:(fun height -> Int64.of_int ((Int.pow 2 (height + 1) - 1) * 4)) heights

let blockSizes = [0x200;0x400;0x800;0x1000;0x2000;0x4000;0x8000;0x10000;0x20000;0x40000;0x80000;0x100000]

let performExperiment oram data iterations =
  let rec loopWrite = function
    | 0 -> Lwt.return (`Ok ())
    | n ->
      O.write oram 0L [data] >>= fun () ->
      loopRead (n - 1)
  and loopRead = function
  | 0 -> Lwt.return (`Ok ())
  | n ->
    O.read oram 0L [data] >>= fun () ->
    loopWrite (n - 1)
  in loopWrite iterations

let () =
  Command.basic
    ~summary:"Run n iterations of Access on ORAMs with heights between x and y and blockSizes between a and b"
    Command.Spec.(
    empty
    +> anon ("iterations" %: int)
    +> anon ("minHeight" %: int)
    +> anon ("maxHeight" %: int))
    (fun iterations minHeight maxHeight () ->
      List.iter (desiredSizes minHeight maxHeight)
                ~f:(fun desiredSizeInSectors ->
                  List.iter blockSizes ~f:(fun desiredBlockSize ->
                    Printf.printf "%Ld, %d, %!" desiredSizeInSectors desiredBlockSize;
                    let oram = connectAndInitialiseORAMOfSize desiredSizeInSectors desiredBlockSize in
                    let info = Lwt_main.run (O.get_info oram) in
                    let sizeInSectors = Int64.to_int_exn info.O.size_sectors in
                    let fullBlockSize = sizeInSectors * info.O.sector_size in
                    let data = Cstruct.create fullBlockSize in
                    let start = Time_ns.now () in
                    begin match Lwt_main.run (performExperiment oram data iterations) with
                    | `Ok () -> ()
                    | `Error _ -> failwith "Failed to perform experiment"
                    end;
                    let time = Time_ns.abs_diff start (Time_ns.now ()) in
                    Printf.printf "%f\n%!" (Time_ns.Span.to_ms time))))
  |> Command.run
