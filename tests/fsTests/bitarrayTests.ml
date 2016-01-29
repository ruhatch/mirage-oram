open Alcotest
open Testable
open Core_kernel.Std

let bitarrayTests =
  [
    "BitarrayCreate_AllFalse_AllFalse", `Quick,
    (fun () ->
      let bitarray = Bitarray.create ~b:false 10 in
      check bool "" true (List.for_all ~f:(fun b -> not b) (Bitarray.to_list bitarray)));
    "BitarrayCreate_AllTrue_AllTrue", `Quick,
    (fun () ->
      let bitarray = Bitarray.create ~b:true 10 in
      check bool "" true (List.for_all ~f:(fun b -> b) (Bitarray.to_list bitarray)));
    "BitarraySet_ZeroToTrue_ZeroTrueAllOtherFalse", `Quick,
    (fun () ->
      let bitarray = Bitarray.create ~b:false 10 in
      check bool "" true (Bitarray.set bitarray 0 true; Bitarray.get bitarray 0 && (List.for_all ~f:(fun b -> not b) (Option.value (List.tl (Bitarray.to_list bitarray)) ~default:[]))));
    "BitarraySet_OneToTrue_OneTrueAllOtherFalse", `Quick,
    (fun () ->
      let bitarray = Bitarray.create ~b:false 10 in
      check bool "" true (Bitarray.set bitarray 1 true; not (Bitarray.get bitarray 0) && Bitarray.get bitarray 1 && (List.for_all ~f:(fun b -> not b) (Option.value (List.tl @@ Option.value (List.tl (Bitarray.to_list bitarray)) ~default:[]) ~default:[]))));
    "BitarraySet_FirstTenToTrue_FirstTenTrueAllOtherFalse", `Quick,
    (fun () ->
      let bitarray = Bitarray.create ~b:false 50 in
      for i = 0 to 9 do
        Bitarray.set bitarray i true;
      done;
      let (first, rest) = List.split_n (Bitarray.to_list bitarray) 10 in
      let first_true = List.for_all ~f:(fun b -> b) first in
      let rest_false = List.for_all ~f:(fun b -> not b) rest in
      check bool "" true (first_true && rest_false));
    "BitarraySet_Quickcheck_SetValueTrue", `Quick,
    (fun () ->
      let bitarray = Bitarray.create ~b:false 1000 in
      Quickcheck.test (Quickcheck.Generator.int_between
                         ~lower_bound:(Incl 0)
                         ~upper_bound:(Excl 1000))
                      (fun index ->
                        Bitarray.set bitarray index true;
                        check bool "" true (Bitarray.get bitarray index)));
  ]

let () =
  Alcotest.run "Bitarray Tests" [
                 "Bitarray Tests", bitarrayTests
               ]
