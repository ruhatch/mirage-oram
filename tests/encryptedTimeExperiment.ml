open Core.Std

module B = BlockSizeController.Make(Block)
module E = Block_ccm.Make(B)
module O = Oram.Make(PosMap.InMemory)(E)

let ( >>= ) x f = Lwt.bind x @@ function
  | `Error e -> Lwt.return (`Error e)
  | `Ok x -> f x

let connectAndInitialiseORAMOfSize desiredSizeInSectors desiredBlockSize =
  match Lwt_main.run (
            Block.connect (Printf.sprintf "diskEnc%Ld.img" desiredSizeInSectors) >>= fun bd ->
            B.connect ~desiredBlockSize bd >>= fun bd ->
            E.connect bd ~key:(Cstruct.of_string "keyofsixteenchar") >>= fun bd ->
            O.create ~desiredSizeInSectors ~desiredBlockSize bd) with
  | `Ok oram -> oram
  | `Error _ -> failwith (Printf.sprintf "Failed to connect to oram with size %Ld" desiredSizeInSectors)

let connectToORAM desiredSizeInSectors desiredBlockSize =
  match Lwt_main.run (
            Block.connect (Printf.sprintf "diskEnc%Ld.img" desiredSizeInSectors) >>= fun bd ->
            B.connect ~desiredBlockSize bd >>= fun bd ->
            E.connect bd ~key:(Cstruct.of_string "keyofsixteenchar") >>= fun bd ->
            O.connect bd) with
  | `Ok oram -> oram
  | `Error _ -> failwith (Printf.sprintf "Failed to connect to oram with size %Ld" desiredSizeInSectors)

let dataForORAM oram =
  let info = Lwt_main.run (O.get_info oram) in
  let pagesPerBlock = (info.O.sector_size - 1) / Io_page.page_size + 1 in
  Io_page.(to_cstruct (get pagesPerBlock))

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
           
(*let performExperiment oram desiredSizeInSectors data iterations =
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
  in loopWrite iterations*)

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
      ignore @@ Nocrypto_entropy_lwt.initialize ();
      List.iter (desiredSizes minHeight maxHeight)
                ~f:(fun desiredSizeInSectors ->
                  Printf.printf "%Ld, %!" desiredSizeInSectors;
                  let oram =
                    if shouldInitialise
                    then connectAndInitialiseORAMOfSize desiredSizeInSectors desiredBlockSize
                    else connectToORAM desiredSizeInSectors desiredBlockSize in
                  let data = dataForORAM oram in
                  let start = Time_ns.now () in
                  begin match Lwt_main.run (performExperiment oram data desiredSizeInSectors iterations) with
                  | `Ok () -> ()
                  | `Error _ -> failwith "Failed to perform experiment"
                  end;
                  let time = Time_ns.abs_diff start (Time_ns.now ()) in
                  Printf.printf "%f\n%!" (Time_ns.Span.to_ms time)))
  |> Command.run
