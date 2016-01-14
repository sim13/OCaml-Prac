open Mp4common

(* From MP3 *)
<<<<<<< .mine
let odd_sum_base = 1;;
=======
>>>>>>> .r1529

let odd_sum_base = 0 ;;

let odd_sum_rec x a =  if  ( x mod 2 =1)  then x + a else a  ;;
let odd_sum l = List.fold_right odd_sum_rec l odd_sum_base ;;

let count_element_base = 0;;
let count_element_rec m a x = if x=m then a+1 else a ;;
let count_element l m = List.fold_left ( count_element_rec m) count_element_base l ;;


let pair_sums_map l = List.map( fun( x,y) -> x+y ) l ;;

let f x = x +1 ;;
let g x = x-1 ;;
let rec apply_even_odd l f g = match l with [] -> [] |  (x :: x1 :: xs)  -> f x :: g x1 :: apply_even_odd xs f g ;;


(* Continutaions *)
(* Problem 1 *)
<<<<<<< .mine
let divk n m k = k ( n/m);;
let modk n m k =  divk n m k ;;
let float_subk a b k =
  raise(Failure "Function not implemented yet.")
let float_mulk a b k =
  raise(Failure "Function not implemented yet.")
let catk str1 str2 k =
  raise(Failure "Function not implemented yet.")
let consk e l k =
  raise(Failure "Function not implemented yet.")
let leqk x y k =
  raise(Failure "Function not implemented yet.")
let eqk x y k =
  raise(Failure "Function not implemented yet.")
=======
let divk n m k = k ( n/m) 
let modk n m k =  k ( n mod m ) 
let float_subk a b k = k ( a-.b)
let float_mulk a b k = k( a *. b)
let catk str1 str2 k = k ( str1 ^ str2) 
let consk e l k = k ( e :: [] ) 
let leqk x y k = k ( x <= y) 
let eqk x y k = k( x= y) 
  
>>>>>>> .r1529

(* Problem 2 *)
let poly x k =    mulk x x ( fun a -> mulk a a ( fun b -> addk b  x  ( fun c -> addk c 1 k  ) ));;
 

(* Problem 3 *)
let distributek f g x y k = f  x  ( fun a -> f y ( fun b -> g a b k ) );;
  

(* Problem 4 *)
<<<<<<< .mine
let rec alternate_series n = if n <= 0 then 0 else   n .*.  -1.0  ** n +.(alternate_series n-1) ;;
let rec alternate_seriesk n k = 
  raise(Failure "Function not implemented yet.")
=======
let rec alternate_series n = if n = 0  || n < 0 then 0 else if n mod 2 = 1 then -n + alternate_series (n-1) else  n + alternate_series (n-1) ;;
let rec alternate_seriesk n k = leqk n 0 ( fun b -> if b then k 0 else subk n 1 ( fun s -> alternate_seriesk s ( fun m ->  modk  n  2 ( fun t -> eqk t 1 ( fun j -> if j then subk m n k else addk n m k )))));;
  
>>>>>>> .r1529

(* Problem 5 *)
let print_intk  i k = k ( print_int i ) ;;
let rec rev_iter f l = match l with [] -> ()| ( x :: xs) -> rev_iter f xs ;; 
let rec rev_iterk f l k =  match l with [] -> () | ( x :: xs ) -> rev_iterk f xs k ;;
 

(* Problem 6 *)
let rec filter l p = match l with []-> [] |( x :: xs ) ->  if   p x = false  then filter xs p   else  if p x then   ( x :: filter xs p ) else []   ;;
let rec filterk l pk k =  match l with [] -> k [] | ( x :: xs ) ->  pk x ( fun m -> if false  then  filterk xs pk k  else if true then k ( x :: xs)  else k [] ) ;;

(* Problem 7 *)
let rec assock l p normalk exceptionk = 
  raise(Failure "Function not implemented yet.")

(* Problem 8 *)
let rec appk l x k =
  raise(Failure "Function not implemented yet.")

