open Core_kernel.Std

type t = int list String.Table.t

let bin_writer_t = String.Table.bin_writer_t (List.bin_writer_t Int.bin_writer_t)

let bin_reader_t = String.Table.bin_reader_t (List.bin_reader_t Int.bin_reader_t)

let create () = String.Table.create ()

let find t word =
match String.Table.find t word with
  | Some result -> result
  | None -> []

let findPhrase t phrase =
  let words = String.split_on_chars phrase ~on:[' '] in
  let postings = List.map ~f:(fun word -> find t word) in
  

let updatePosting t word docId =
  let posting = find t word in
  let _ = String.Table.set t ~key:word ~data:(docId :: posting) in
  ()

let indexFile t fileContents docId =
  let fileString = Cstruct.to_string fileContents in
  let words = String.split_on_chars fileString ~on:[' '] in
  List.iter ~f:(fun word -> updatePosting t word docId) words

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
