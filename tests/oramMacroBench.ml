open Core.Std

module O = Oram.Make(PosMap.InMemory)(Block)

let ( >>= ) x f = Lwt.bind x @@ function
  | `Error e -> Lwt.return (`Error e)
  | `Ok x -> f x

let connectAndInitialiseORAMOfSize desiredSizeInSectors desiredBlockSize =
  match Lwt_main.run (
            Block.connect (Printf.sprintf "disk%Ld.img" desiredSizeInSectors) >>= fun bd ->
            O.create ~desiredSizeInSectors ~desiredBlockSize bd) with
  | `Ok oram -> oram
  | `Error (`Unknown s) -> failwith s
  | `Error `Disconnected -> failwith "Disconnected!"
  | `Error `Is_read_only -> failwith "Read only?!"
  | `Error _ -> failwith (Printf.sprintf "Failed to connect to oram with size %Ld" desiredSizeInSectors)

let connectToORAM desiredSizeInSectors =
  match Lwt_main.run (
            Block.connect (Printf.sprintf "disk%Ld.img" desiredSizeInSectors) >>= fun bd ->
            O.connect bd) with
  | `Ok oram -> oram
  | `Error (`Unknown s) -> failwith s
  | `Error `Disconnected -> failwith "Disconnected!"
  | `Error `Is_read_only -> failwith "Read only?!"
  | `Error _ -> failwith (Printf.sprintf "Failed to connect to oram with size %Ld" desiredSizeInSectors)

let dataForORAM oram =
  let info = Lwt_main.run (O.get_info oram) in
  O.createPageAlignedBuffer info.O.sector_size

let desiredSizes minHeight maxHeight =
  let heights = List.range minHeight maxHeight in
  List.map ~f:(fun height -> Int64.of_int ((Int.pow 2 (height + 1) - 1) * 4)) heights

let performExperiment oram data desiredSizeInSectors iterations =
  let rec loopWrite = function
    | 0 -> Lwt.return (`Ok ())
    | n ->
      let address = Random.int64 desiredSizeInSectors in
      O.write oram address [data] >>= fun () ->
      loopRead (n - 1)
  and loopRead = function
  | 0 -> Lwt.return (`Ok ())
  | n ->
    let address = Random.int64 desiredSizeInSectors in
    O.read oram address [data] >>= fun () ->
    loopWrite (n - 1)
  in loopWrite iterations

(*  let reverseOperation = function
    | O.Read -> O.Write
    | O.Write -> O.Read
  in
  let rec loop address operation = function
    | 0 -> Lwt.return (`Ok ())
    | n ->
       O.access oram operation address data >>= fun _ ->
       if address = Int64.(desiredSizeInSectors - 1L)
       then loop 0L (reverseOperation operation) (n - 1)
       else loop Int64.(address + 1L) operation (n - 1)
  in loop 0L O.Write iterations*)

let () =
  Command.basic
    ~summary:"Run n iterations of Access on ORAMs with heights between x and y"
    Command.Spec.(
    empty
    +> anon ("iterations" %: int)
    +> anon ("minHeight" %: int)
    +> anon ("maxHeight" %: int)
    +> flag "i" no_arg ~doc:"Pass in this flag to initialise block devices before connecting"
    +> flag "b" (optional_with_default 1048576 int) ~doc:"Optional block size parameter")
    (fun iterations minHeight maxHeight shouldInitialise desiredBlockSize () ->
      List.iter (desiredSizes minHeight maxHeight)
                ~f:(fun desiredSizeInSectors ->
                  Printf.printf "%Ld, %!" desiredSizeInSectors;
                  let oram =
                    if shouldInitialise
                    then connectAndInitialiseORAMOfSize desiredSizeInSectors desiredBlockSize
                    else connectToORAM desiredSizeInSectors in
                  let data = dataForORAM oram in
                  let start = Time_ns.now () in
                  begin match Lwt_main.run (performExperiment oram data desiredSizeInSectors iterations) with
                  | `Ok () -> ()
                  | `Error _ -> failwith "Failed to perform experiment"
                  end;
                  let time = Time_ns.abs_diff start (Time_ns.now ()) in
                  Printf.printf "%f\n%!" (Time_ns.Span.to_ms time)))
  |> Command.run
