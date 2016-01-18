open Core_kernel.Std

type t = String.Hash_set.t String.Table.t

let bin_writer_t = String.Table.bin_writer_t (String.Hash_set.bin_writer_t)

let bin_reader_t = String.Table.bin_reader_t (String.Hash_set.bin_reader_t)

let create () = String.Table.create ()

let intersectPostings postings1 postings2 =
  Hash_set.filter postings1 ~f:(fun elem -> Hash_set.mem postings2 elem)

let find t word = String.Table.find t word

let findPhrase t phrase =
  let words = String.split_on_chars phrase ~on:[' '] in
  let postings = List.filter_opt (List.map ~f:(fun word -> find t word) words) in
  List.reduce postings ~f:intersectPostings

let fileNamesForQuery t query =
  match findPhrase t query with
    | Some result -> Hash_set.to_list result
    | None -> []

let updatePosting t word fileName =
  Printf.printf "Updating posting for %s with file name %s\n" word fileName;
  match find t word with
    | Some posting -> Hash_set.add posting fileName
    | None ->
      let data = String.Hash_set.create () in
      Hash_set.add data fileName;
      String.Table.set t ~key:word ~data

let stripFile = String.filter ~f:(fun a -> not (String.contains ".,;:'\"\'?!" a))

let indexFile t name contents =
  let fileString = stripFile (Cstruct.to_string contents) in
  let words = List.dedup (String.split_on_chars fileString ~on:[' ';'\n';'\r';'-']) in
  List.iter ~f:(fun word -> updatePosting t word name) words

let toCstruct t blockSize =
  let binarySize = bin_writer_t.Bin_prot.Type_class.size t in
  let requiredBlocks = (binarySize + 8 - 1) / blockSize + 1 in
  let bufferSize = requiredBlocks * blockSize in
  let buffer = Bin_prot.Common.create_buf bufferSize in
  let _ = bin_writer_t.Bin_prot.Type_class.write buffer ~pos:8 t in
  let result = Cstruct.of_bigarray buffer in
  Cstruct.LE.set_uint64 result 0 (Int64.of_int binarySize);
  result

let ofCstruct cstruct =
  let binarySize = Int64.to_int_exn (Cstruct.LE.get_uint64 cstruct 0) in
  let binaryCstruct = Cstruct.sub cstruct 8 binarySize in
  let buffer = Cstruct.to_bigarray binaryCstruct in
  bin_reader_t.Bin_prot.Type_class.read buffer ~pos_ref:(ref 0)
