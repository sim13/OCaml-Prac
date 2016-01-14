(* CS421 - Fall 2014
 * MP3
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)

open List

(* Problem 1 *)
let rec get_nth l n = match l with [] -> [] | ( x :: xs) -> if n = 0 then [x] else if n < 0 then [] else get_nth xs (n-1) ;;
 
(* Problem 2 *)
let rec count_change money coins = match coins with [] -> 0 | (x :: xs) -> if money < 0 then  0 else money -1 ;;
  
(* Problem 3 *)
let rec elems_in_interval l i = match  l , i with [] , (_,_)  -> 0  | (x :: xs) , (a,b)  -> if (a <= x && b >= x ) then 1 +  elems_in_interval xs ( a, b)  else 0 + elems_in_interval xs (a,b) ;;

(*let rec elems_in_interval l i = raise ( Failure " Function not implemented yet ");;*)
(* Problem 4 *)
let rec check_if_all_pos l = match l with [] -> false | (x :: xs) -> if x > 0 then true else check_if_all_pos xs ;;

(* Problem 5 *)
let  add_tuple (x,y,z,w)  = x+ y + z+ w ;;
let rec sum_quads qs = match qs with []-> []  | ( x :: xs) -> add_tuple (x)  :: sum_quads xs ;;
(*let rec sum_quads qs = raise( Failure " Function not implemented yet.");;*)

(* Problem 6 *)
(*let rec ret_pair x =  if x mod 2 = 0  then (0,1) else (1,0) ;;
let rec evens_odds l = match l with [] -> (0,0)| ( x :: xs)  -> if x mod 2 = 0 then ( 0 + ret_pair xs , 1 + ret_pair xs) else ( 1 + ret_pair xs  , 0 + ret_pair xs) ;;*)
let rec evens_odds l = raise  (Failure "Function not implemented yet.");;


(* Problem 7 *)
let rec inc_evens_dec_odds l = raise(Failure "Function not implemented yet.");;

(* Problem 8 *)
let rec odd_sum l = raise(Failure "Function not implemented yet.");;

(* Problem 9 *)
let rec pair_sums l = raise(Failure "Function not implemented yet.");;

(* Problem 10 *)
let rec count_element l m = raise(Failure "Function not implemented yet.");;

(* Problem 11 *)
let rec rev l = raise(Failure "Function not implemented yet.");;

let rec merge_pairs l = raise(Failure "Function not implemented yet.");;

(* Problem 12 *)
let odd_sum_base = 1;; (* You may need to change this *)

let odd_sum_rec x a = raise(Failure "Function not implemented yet.");;

(* Problem 13 *)
let count_element_base = 1;; (* You may need to change this *)

let count_element_rec m a x = raise(Failure "Function not implemented yet.");;

(* Problem 14 *)
let pair_sums_map l = raise(Failure "Function not implemented yet.");;

(* Problem 15 *)
let rec apply_even_odd l f g = raise(Failure "Function not implemented yet.");;

