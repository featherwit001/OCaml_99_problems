(** <29> Determine Whether a Given Integer Number Is Prime  *)
(* Require: [x] is integer which is bigger than 1 *)
let is_prime n = 
  if n < 2 then false 
  else
    let res = ref true in
    let i = ref 2 in
    (* !i * !i <= n ;  sqrt n *)
    while ( !i <= n / !i) do 
      if n mod !i = 0 then res := false;
      i := !i + 1;
    done;
    !res

(** <30> Determine the Greatest Common Divisor of Two Positive Integer Numbers *)
(* / in OCaml make result closer to zero *)
(* a mod b = a - b * floor (a /. b) *)
let rec gcd a b =
  if b = 0 then a
  else gcd b (a mod b)

(** <31> Determine Whether Two Positive Integer Numbers Are Coprime *)
let coprime a b = 
  gcd a b = 1


(** <32> Calculate Euler's Totient Function Φ(m) *)
let phi n = 
  let rec phi_aux acc i =
    if i <= n then
      phi_aux (if coprime i n then acc + 1 else acc) (i + 1)
    else acc
  in
  if n = 1 then 1 else phi_aux 0 1
  (* why it from 1 *)


(** <33> Determine the Prime Factors of a Given Positive Integer   *)
let factors n =
  let lst = ref [] in
  let n = ref n in
  let i = ref 2 in

  while ( !i <= !n / !i) do 
    if( !n mod !i = 0) then begin
      while (!n mod !i = 0) do 
        n := !n / !i;
        lst := !i :: !lst
      done;
    end;
    i := !i + 1
  done;
  if !n > 1 then lst := !n :: !lst;
  List.rev !lst


(** <34> Determine the Prime Factors of a Given Positive Integer (2)  *)
let factors2 n = 
  let n = ref n in
  let lst = ref [] in
  let i = ref 2 in
  while !i <= !n / !i do
    if !n mod !i  = 0 then begin
        let t = ref 0 in
        while !n mod !i = 0 do
          n := !n / !i;
          t := !t + 1 
        done;
        lst := (!i, !t) :: !lst
      end;
    i := !i + 1
  done;
  if !n > 1 then lst := (!n, 1) :: !lst;
  List.rev !lst


(** <35> Calculate Euler's Totient Function Φ(m) (Improved)  
     N = (p1 ^ a1) * (p2 ^ a2) *.... *(Pn ^ an) eg. 6 = 2 * 3 
    phi N = N * (1 - 1 / p1) * (1 - 1 / p2) * ... * (1 - 1 / pn) *)
let phi_improved x = 
  (* res = N  *)
  let res = ref x in  
  let x = ref x in
  let i = ref 2 in
  (* i * i <= x *)
  while !i <= !x / !i do
    if !res mod !i = 0 then begin 
      (* res * (1 - 1 / p1) *)
      res := !res / !i * (!i - 1);
      while (!x mod !i = 0) do x := !x / !i done
    end;
    i := !i + 1;
  done ;
  (* the only one factor greater than (sqrt n ) *)
  if !x > 1 then res := !res / !x *(!x - 1);
  !res

(* <36> Compare the Two Methods of Calculating Euler's Totient Function *)
let timeit f n =
  let t0 = Unix.gettimeofday () in
  ignore(f n);
  let t1 = Unix.gettimeofday () in
   t1 -. t0

let rec time_compare_aux acc count f1 f2 n =
  if n = 0 then acc /. float(count)
  else 
    let time1 = timeit f1 n in
    let time2 = timeit f2 n in
    let acc' = time1 -. time2 +. acc in
    time_compare_aux acc' count f1 f2 (n - 1) 


let time_compare ?(count = 5) f1 f2 n =
  time_compare_aux 0. count f1 f2 n
  

(** <37>  A List of Prime Numbers 
    Require: a must be 2*)
let all_primes _a n = 
  let primes = Array.make (n+10) 0 in 
  let count = ref 0 in
  let st = Array.make (n+10) false in 
  let i = ref 2 in
  while !i <= n do
    if st.(!i) = false then begin
      primes.(!count) <- !i;
      count := !count + 1;
      end;
    (* Printf.printf "i = %d filter: " !i; *)
    let j = ref 0 in
    let quit_loop = ref false in 
    while (primes.(!j) <= n / !i ) &&(not !quit_loop) 
    do
      st.(primes.(!j) * !i) <- true;
      (* Printf.printf "%d " (primes.(!j) * !i); *)
      if !i mod primes.(!j) = 0 then quit_loop := true ;
      j := !j + 1
    done;
    (* Printf.printf "end\n"; *)
    i := !i + 1
  done;
  Array.to_list (Array.sub primes 0 !count)


(* <38> Goldbach's Conjecture 
   Require: n must be even positive number. *)
let goldbach n = 
  let module Pset = Set.Make(Int) in 
  let plst = all_primes 2 n in 
  let pset = Pset.of_list plst in
  let rec goldbach_aux = function
    | [] -> failwith "may be not in mathematics"
    | h :: t -> if Pset.mem (n - h) pset then (h, n - h) else goldbach_aux t
  in
  goldbach_aux plst

(** <39> A List of Goldbach Compositions 
    Require: a <= b*)
let goldbach_list a b = 
  let module Pset = Set.Make(Int) in
  let plst = all_primes 2 b in 
  let pset = Pset.of_list plst in 
  let rec goldbach_aux n = function
    | [] -> failwith "may be not in mathematics"
    | h :: t -> if Pset.mem (n - h) pset then (h, n - h) else goldbach_aux n t
  in
  let rec goldbach_list_aux acc a b = 
    if a > b then acc
    else 
      goldbach_list_aux (if a mod 2 = 0 then (a, goldbach_aux a plst) :: acc else acc) (a + 1) b
  in 
    List.rev (goldbach_list_aux [] a b)
    
  
    