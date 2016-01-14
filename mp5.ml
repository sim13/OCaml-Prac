(* File: mp5-skeleton.ml *)

open Mp5common

(* Problem 1 *)
let rec import_list lst = match  lst with [] -> ConstExp NilConst | (  ( a, b)  :: xs  ) -> BinOpAppExp( ConsOp , BinOpAppExp(CommaOp , ConstExp ( IntConst a ) , ConstExp ( IntConst b ) ) , import_list xs ) ;;

(* Problem 2 *)
let pair_sum = IfExp ( VarExp "[]" , ConstExp NilConst ,BinOpAppExp(IntPlusOp , VarExp "x1" , VarExp "x2") ) ;;
(* Problem 3 *)
let max a b = if a >=b then a else b 
let max2 f a b = max ( f a ) ( f b )
let rec cal_max_exp_height exp =  match exp with 
 VarExp _ -> 1 | 
 ConstExp _-> 1 |
 MonOpAppExp ( mon_op , exp1 ) -> 1 + cal_max_exp_height exp1 |
 BinOpAppExp ( bin_op , exp1 , exp2 ) -> 1 + max2 cal_max_exp_height exp1 exp2 ;;
(* Problem 4 *)
let rec freeVarsInExp exp = match exp with
 VarExp(x) -> [x]
|ConstExp _ -> []  
|IfExp(exp1 , exp2 , exp3 ) ->  freeVarsInExp exp1 @ freeVarsInExp exp2 @ freeVarsInExp exp3 
|FunExp (x , exp1 )->   let l = freeVarsInExp exp1  in List.filter( fun x-> x!= x) l 
|LetInExp(x,e1,e2) ->  let l2 = freeVarsInExp e2 in freeVarsInExp e1 @ List.filter ( fun x -> x!=x) l2 
|LetRecInExp (f,x,e1,e2) ->  let l1 = freeVarsInExp e1  in  let l2 = freeVarsInExp e2  in  (List.filter ( fun z -> not (z=x)) ( List.filter( fun z-> not(z=f))l1 )) @ List.filter ( fun y -> not( y=f))l2 ;;
(* Problem 5 *)
let rec cps_exp e k kx =  raise ( Failure " Function not implemented yet")

