open Core.Std

module O = Oram.Make(PosMap.InMemory)(Block)

let ( >>= ) x f = Lwt.bind x @@ function
  | `Error e -> Lwt.return (`Error e)
  | `Ok x -> f x

let connectAndInitialiseORAMOfSize desiredSizeInSectors desiredBlockSize =
  match Lwt_main.run (
            Block.connect "diskBlock.img" >>= fun bd ->
            O.create ~desiredSizeInSectors ~desiredBlockSize bd) with
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

let blockSizes = [(*0x200;0x400;0x800;0x1000;*)0x2000;0x4000;0x8000;0x10000;0x20000;0x40000;0x80000;0x100000;0x200000;0x400000;0x800000]

let rec computeIterations iterationAbove acc = function
| [] -> failwith "Can't computer iterations on empty list"
| [_] -> acc
| (x1 :: x2 :: xs) ->
  let multiplier = Float.(of_int x1 / of_int x2) in
  let exactIteration = Float.(iterationAbove * multiplier) in
  let iterationToUse = Float.iround_exn exactIteration in
  computeIterations exactIteration (iterationToUse :: acc) (x2 :: xs)

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

let () =
  Command.basic
    ~summary:"Run block size experiment, performing n iterations on largest block size on ORAMs with heights between x and y"
    Command.Spec.(
    empty
    +> anon ("iterations" %: int)
    +> anon ("minHeight" %: int)
    +> anon ("maxHeight" %: int))
    (fun iterations minHeight maxHeight () ->
      let iterations = computeIterations (Float.of_int iterations) [iterations] (List.map ~f:(fun b -> b - 8) (List.rev blockSizes)) in
      let testCases = List.zip_exn blockSizes iterations in
      List.iter (desiredSizes minHeight maxHeight)
                ~f:(fun desiredSizeInSectors ->
                  List.iter testCases ~f:(fun (desiredBlockSize, iterations) ->
                    Printf.printf "%Ld, %d, %!" desiredSizeInSectors desiredBlockSize;
                    let oram = connectAndInitialiseORAMOfSize desiredSizeInSectors desiredBlockSize in
                    let info = Lwt_main.run (O.get_info oram) in
                    let data = Cstruct.create info.O.sector_size in
                    let start = Time_ns.now () in
                    begin match Lwt_main.run (performExperiment oram data desiredSizeInSectors iterations) with
                    | `Ok () -> ()
                    | `Error _ -> failwith "Failed to perform experiment"
                    end;
                    let time = Time_ns.abs_diff start (Time_ns.now ()) in
                    Printf.printf "%f\n%!" (Time_ns.Span.to_ms time))))
  |> Command.run
