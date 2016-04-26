open Core.Std
open PosMapIntf

let rec buildPosmap desiredSizeInSectors =
  let b = Int64.(desiredSizeInSectors / 5L) in
  if b < 0L
  then (module PosMap.InMemory : PosMapF)
  else (module Oram.Make((val (buildPosmap desiredSizeInSectors))) : PosMapF)

let oramModule desiredSizeInSectors =
  let module P = (val (buildPosmap desiredSizeInSectors)) in
  (module Oram.Make(P)(Block) : Oram.ORAM with type blockDevice = Block.t and type blockError = Block.error)

let ( >>= ) x f = Lwt.bind x @@ function
  | `Error e -> Lwt.return (`Error e)
  | `Ok x -> f x

let desiredSizes minHeight maxHeight =
  let heights = List.range minHeight maxHeight in
  List.map ~f:(fun height -> Int64.of_int ((Int.pow 2 (height + 1) - 1) * 4)) heights

module ExperimentF (O : Oram.ORAM with type blockDevice = Block.t and type blockError = Block.error) = struct

  let connectAndInitialiseORAMOfSize desiredSizeInSectors =
    match Lwt_main.run (
              Block.connect (Printf.sprintf "disk%Ld.img" desiredSizeInSectors) >>= fun bd ->
              O.create bd) with
    | `Ok oram -> oram
    | `Error _ -> failwith (Printf.sprintf "Failed to connect to oram with size %Ld" desiredSizeInSectors)
                           
  let connectToORAM desiredSizeInSectors =
    match Lwt_main.run (
              Block.connect (Printf.sprintf "disk%Ld.img" desiredSizeInSectors) >>= fun bd ->
              O.connect bd) with
    | `Ok oram -> oram
    | `Error _ -> failwith (Printf.sprintf "Failed to connect to oram with size %Ld" desiredSizeInSectors)
                           
  let dataForORAM oram =
    let info = Lwt_main.run (O.get_info oram) in
    Some (Cstruct.create (info.O.sector_size))
         
  let performExperiment oram desiredSizeInSectors data iterations =
    let reverseOperation = function
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
    in loop 0L O.Write iterations
            
end
                                      
let () =
  Command.basic
    ~summary:"Run n iterations of Access on ORAMs with heights between x and y"
    Command.Spec.(
    empty
    +> anon ("iterations" %: int)
    +> anon ("minHeight" %: int)
    +> anon ("maxHeight" %: int)
    +> flag "i" no_arg ~doc:"Pass in this flag to initialise block devices before connecting")
    (fun iterations minHeight maxHeight shouldInitialise () ->
      List.iter (desiredSizes minHeight maxHeight)
                ~f:(fun desiredSizeInSectors ->
                  Printf.printf "%Ld, %!" desiredSizeInSectors;
                  let module O = (val oramModule(desiredSizeInSectors)) in
                  let module Experiment = ExperimentF(O) in
                  let oram =
                    if shouldInitialise
                    then Experiment.connectAndInitialiseORAMOfSize desiredSizeInSectors
                    else Experiment.connectToORAM desiredSizeInSectors in
                  let data = Experiment.dataForORAM oram in
                  let start = Time_ns.now () in
                  begin match Lwt_main.run (Experiment.performExperiment oram desiredSizeInSectors data iterations) with
                  | `Ok () -> ()
                  | `Error _ -> failwith "Failed to perform experiment"
                  end;
                  let time = Time_ns.abs_diff start (Time_ns.now ()) in
                  Printf.printf "%f\n%!" (Time_ns.Span.to_ms time)))
  |> Command.run
       
