let calcL0 blockExp sizeSectors =
  let k = 1. +. 2. ** (6. -. blockExp) +. 2. ** (12. -. 2. *. blockExp) +. 2. ** (18. -. 3. *. blockExp) +. 2. ** (24. -. 4. *. blockExp) in
  let term1 = 5. *. log 2. +. sizeSectors *. log 2. +. (log (k *. log 2.)) in
  let term2 = log term1 in
  let term3 = log (1. -. term2 /. term1) in
  let whole = term2 +. term3 -. (log (k *. log 2.)) in
  floor (whole /. log 2.)

let rec calcLevelList blockExp ln n =
  if ln > 0.
    then ln :: calcLevelList blockExp (ln +. 6. -. blockExp) (n +. 1.)
    else []

let calcUsedSpace (x::xs) = List.fold_left (fun acc l -> acc +. (2. ** l) -. 1.) x (x::xs)

let printLevels lvls =
  let rec lvlNums acc = function
    | 0 -> 0 :: acc
    | n -> lvlNums (n :: acc) (n - 1) in
  List.iter2 (fun l n -> Printf.printf "L%d: %d\n" n (int_of_float l)) lvls (lvlNums [] (List.length lvls - 1));
  Printf.printf "Used space: %f\n" (calcUsedSpace lvls)

let testLevelCalc blockExp sizeSectors =
  let l0 = calcL0 blockExp sizeSectors in
  let lvls = calcLevelList blockExp l0 0. in
  assert (calcUsedSpace lvls < sizeSectors);
  printLevels lvls

let () =
  for i = 1 to 100 do
    let sizeSectors = Random.float 10000000000000. in
    testLevelCalc 20. sizeSectors
  done
