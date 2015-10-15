open V1_LWT
open Printf
open Core_kernel

module Make (B: BLOCK) = struct

  include B

  type op =
    | Read
    | Write

  let oram = Bucket_tree.empty B.size_sectors (* This will not be present in the real implementation, this will actually be mapped in memory, so will just be calculations *)

  let stash = Stash.create ()

  let position = Array.create ~len:(Bucket_tree.size oram * 3) 0

  let access op a data' =
    let x = position.(a) in
    position.(a) <- Random.int (Bucket_tree.leaves oram);
    List.iter (Bucket_tree.read_path_to_leaf oram x) ~f:(fun (b1, b2, b3) ->
      Stash.add stash b1;
      Stash.add stash b2;
      Stash.add stash b3);
    let data = Stash.find_index stash (Some a) in
    begin match op with
      | Write -> Hash_set.remove stash (Some a,data);
                 Hash_set.add stash (Some a,Option.value data' ~default:"")
      | Read -> ()
    end;
    let take_from_stash l =
      match Hash_set.find stash ~f:(fun (a',_) ->
        match a' with
          | Some x' -> Bucket_tree.path_intercept x position.(x') (Bucket_tree.height oram) l
          | None -> false)
      with
        | Some b -> Hash_set.remove stash b;
                    b
        | None -> Block.empty
    in
    let build_bucket l = (take_from_stash l, take_from_stash l, take_from_stash l) in
    let rec build_path acc = function
      | 0 -> (build_bucket 0) :: acc
      | x -> build_path ((build_bucket x) :: acc) (x - 1)
    in
    Bucket_tree.write_path_to_leaf_exn oram x (build_path [] (Bucket_tree.height oram));
    data

  let write x sector_start buffers = B.write a

  let read_to_buffer x sector_start b =
    let len = b.Cstruct.len / x.info.sector_size in
    let rec loop = function
      | 0 -> return (`Ok ())
      | x ->
        let data = access Read (sector_start + x - 1) None in
        Cstruct.blit data 0 b ((x - 1) * x.info.sector_size) x.info.sector_size;
        loop (x - 1)
    in loop len


  let rec read x sector_start = function
    | [] -> return (`Ok ())
    | b :: bs ->
      read_to_buffer x sector_start b >>= function
        | `Ok () ->
          read x Int64.(add sector_start (of_int (b.Cstruct.len / x.info.sector_size))) bs
        | `Error x ->
          return (`Error x)

end
