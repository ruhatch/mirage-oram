open Core.Std
open Testable

module B = BlockSizeController.Make(Block)

let connectToBlockDevice desiredSizeInSectors =
  match Lwt_main.run (
            Block.connect "diskControl.img" >>= fun bd ->
            B.connect bd) with
  | `Ok blockDevice -> blockDevice
  | `Error _ -> failwith (Printf.sprintf "Failed to connect to oram with size %Ld" desiredSizeInSectors)

let dataForBlockDevice blockDevice =
  let info = Lwt_main.run (B.get_info blockDevice) in
  Cstruct.create (info.B.sector_size)

let desiredSizes minHeight maxHeight =
  let heights = List.range minHeight maxHeight in
  List.map ~f:(fun height -> Int64.of_int ((Int.pow 2 (height + 1) - 1) * 4)) heights

let performExperiment blockDevice desiredSizeInSectors data iterations =
  let reverseOperation = function
    | O.Read -> O.Write
    | O.Write -> O.Read
  in
  let rec loop address operation = function
    | 0 -> Lwt.return (`Ok ())
    | n ->
       begin match operation with
       | O.Write -> B.write blockDevice address [data]
       | O.Read -> B.read blockDevice address [data]
       end >>= fun _ ->
       if address = Int64.(desiredSizeInSectors - 1L)
       then loop 0L (reverseOperation operation) (n - 1)
       else loop Int64.(address + 1L) operation (n - 1)
  in loop 0L O.Write iterations

let () =
  Command.basic
    ~summary:"Run n iterations of Access on ORAMs with heights between x and y"
    Command.Spec.(
    empty
    +> anon ("iterations" %: int)
    +> anon ("minHeight" %: int)
    +> anon ("maxHeight" %: int))
    (fun iterations minHeight maxHeight () ->
      List.iter (desiredSizes minHeight maxHeight)
                ~f:(fun desiredSizeInSectors ->
                  Printf.printf "%Ld, %!" desiredSizeInSectors;
                  let blockDevice = connectToBlockDevice desiredSizeInSectors in
                  let data = dataForBlockDevice blockDevice in
                  let start = Time_ns.now () in
                  begin match Lwt_main.run (performExperiment blockDevice desiredSizeInSectors data iterations) with
                  | `Ok () -> ()
                  | `Error _ -> failwith "Failed to perform experiment"
                  end;
                  let time = Time_ns.abs_diff start (Time_ns.now ()) in
                  Printf.printf "%f\n%!" (Time_ns.Span.to_ms time)))
  |> Command.run
