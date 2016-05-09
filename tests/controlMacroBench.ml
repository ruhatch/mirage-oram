open Core.Std
open Testable

module B = BlockSizeController.Make(Block)

let connectToBlockDevice desiredSizeInSectors desiredBlockSize =
  match Lwt_main.run (
            Block.connect "diskControl.img" >>= fun bd ->
            B.connect ~desiredBlockSize bd) with
  | `Ok blockDevice -> blockDevice
  | `Error _ -> failwith (Printf.sprintf "Failed to connect to oram with size %Ld" desiredSizeInSectors)

let dataForBlockDevice blockDevice =
  let info = Lwt_main.run (B.get_info blockDevice) in
  let pagesPerBlock = info.B.sector_size / Io_page.page_size in
  Io_page.(to_cstruct (get pagesPerBlock))

let desiredSizes minHeight maxHeight =
  let heights = List.range minHeight maxHeight in
  List.map ~f:(fun height -> Int64.of_int ((Int.pow 2 (height + 1) - 1) * 4)) heights

(*let performExperiment oram data desiredSizeInSectors iterations =
  let rec loopWrite = function
    | 0 -> Lwt.return (`Ok ())
    | n ->
      let address = Random.int64 desiredSizeInSectors in
      B.write oram address [data] >>= fun () ->
      loopRead (n - 1)
  and loopRead = function
  | 0 -> Lwt.return (`Ok ())
  | n ->
    let address = Random.int64 desiredSizeInSectors in
    B.read oram address [data] >>= fun () ->
    loopWrite (n - 1)
  in loopWrite iterations*)

let performExperiment oram data desiredSizeInSectors iterations =
  let rec loop address write = function
    | 0 -> Lwt.return (`Ok ())
    | n ->
       begin
         if write
         then B.write oram address [data]
         else B.read oram address [data]
       end >>= fun () ->
       if address = Int64.(desiredSizeInSectors - 1L)
       then loop 0L (not write) (n - 1)
       else loop Int64.(address + 1L) write (n - 1)
  in loop 0L true iterations

let () =
  Command.basic
    ~summary:"Run n iterations of Access on ORAMs with heights between x and y"
    Command.Spec.(
    empty
    +> anon ("iterations" %: int)
    +> anon ("minHeight" %: int)
    +> anon ("maxHeight" %: int)
    +> flag "b" (optional_with_default 1048576 int) ~doc:"Optional block size parameter")
    (fun iterations minHeight maxHeight desiredBlockSize () ->
      List.iter (desiredSizes minHeight maxHeight)
                ~f:(fun desiredSizeInSectors ->
                  (*Printf.printf "%Ld, %!" desiredSizeInSectors;*)
                  let blockDevice = connectToBlockDevice desiredSizeInSectors desiredBlockSize in
                  let data = dataForBlockDevice blockDevice in
                  (*let start = Time_ns.now () in*)
                  begin match Lwt_main.run (performExperiment blockDevice data desiredSizeInSectors iterations) with
                  | `Ok () -> ()
                  | `Error _ -> failwith "Failed to perform experiment"
                  end(*;
                  let time = Time_ns.abs_diff start (Time_ns.now ()) in
                  Printf.printf "%f\n%!" (Time_ns.Span.to_ms time)*)))
  |> Command.run
