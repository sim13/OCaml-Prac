(* File: mp4common.ml *)

let output_str = ref "";;

let print_string str = 
  (output_str := !output_str ^ str;
  Pervasives.print_string (str))

let print_int n = print_string (string_of_int n)
let print_float x = print_string (string_of_float x)
let print_newline () = print_string "\n";;
let print_endline s = print_string (s^"\n");;

let report x =
   print_string "Result: ";
   print_int x;
   print_newline();;

let addk x y k = k (x + y)
let subk x y k = k (x - y)
let mulk x y k = k (x * y)
let float_addk x y k = k(x +. y)
let float_divk x y k = k(x /. y)
let pairk x y k = k(x,y)

