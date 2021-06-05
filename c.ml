open Printf
open List
let n = read_int() (*number of lines*)

(* Function greedy algotithm ======================== *)
let rec search_max m l c =
  let v = List.nth l (c-1) in 
  if (v > m)
  then search_max m l (c-1)
  else v
  
let rec greedy l m c =
  if (m > 0)
  then 
  (
    let v = search_max m l (length l) in
    greedy l (m-v) (c+1)
  )
  else (printf"G: %i" c;c)

(* Dynamic function =============================== *)
let dynamic l m =
  
  let t = Array.make (m+1) max_int in
  t.(0) <- 0;
  (* Array.iter (printf "%d ") t *)
  let num_coins = (length l) - 1 in 
  for i = 1 to m do
    (* printf "(i---) %i\n" i; *)
    for j = 0 to num_coins do

      (* printf "(j) %i\n" j; *)
      let coin = nth l j in
      if (coin <= i)
      then 
      (
        let sub_res = t.(i-coin) in
        if (sub_res <> max_int && sub_res + 1 < t.(i))
        then 
        (
          t.(i) <- sub_res + 1
        )
        else ()
      )
      else ()

    done;

  done;
  (* Array.iter (printf "%d ") t; *)
  match t.(m) with
  (* | max_int -> printf"\nMaximo valor\n" *)
  | _ -> printf" || D: %i\n" t.(m);t.(m);;


(* Initialise function =============================== *)
let rec input n c l =
  if (c < n) 
  then 
  (
    let la = read_int() in
    input n (c+1) (l@[la])
  )
  else (l)

let rec funcio l r1 r2 v = 
  if (r1 < r2 && v == 0)
  then 
  (
    let g = greedy l r1 0 
    and d = dynamic l r1 
    in 
    if (g == d)
    then (funcio l (r1+1) r2 0)
    else (funcio l (r1+1) r2 r1)
  )
  else (v);;

let change_making_problem l =
  if (length l >= 3)
  then 
  (
    let r1 = (List.nth l 2) + 2
    and r2 = (List.nth l ((length l) - 1)) + (List.nth l ((length l) - 2)) 
    in printf"R1: %i R2: %i\n" r1 r2;
    funcio l r1 r2 0 
  )
  else (0)

let main n =
  let l = input n 0 [] in
  let result = change_making_problem l in
  if (result == 0)
  then (printf"YES\n")
  else (printf"%i\n" result);;

let () = main n;;
  
(* Exemplos
  3
  1
  3
  4
  R: 6
=======
  3
  1 
  5 
  12
  R: 15
*)