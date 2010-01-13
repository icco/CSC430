(**
 * A set of functions for Assignment #1: ML Introduction.
 *
 * @author Nathaniel "Nat" Welch
 *)

(* Part 1 *)
fun number_of v [] = 0
  | number_of v (h::l) = 
   if h = v then
     1 + number_of v l
   else
     0 + number_of v l 
;

(* Part 2 *)
fun pair_swap [] = []
  | pair_swap ((x, y)::zs):('a * 'a) list =
   (y, x)::zs
;

(* Part 3 *)
exception ImbalancedWeaving;
fun weave [] [] = []
  | weave (x::[]) [] =
   x::[]
  | weave [] y = 
   raise ImbalancedWeaving
  | weave x [] = 
   raise ImbalancedWeaving
  | weave (x::xs) (y::ys) =
   x::y::weave xs ys
;

(*
(* Part 4 *)
open TextIO;
fun which_char x [] = x
  | which_char x ((y,z)::xs) = 
  if x = y then 
    which_char z xs 
  else 
    which_char x xs
;

fun filept_subst infile l =
   if endOfStream infile then []
   else (which_char(inputN(infile,1) l)::(filept_subst infile l)
;

fun print_array [] = nil
  | print_array (x::xs) =
   print x
   print_array xs
;

fun file_subst file_name [] = []
  | file_subst file_name l =
   print_array (filept_subst (openIn file_name) l)
;
*)

(* Part 5 *)
datatype 'a ThingCollection =
     OneThing of ('a * 'a ThingCollection)
   | TwoThings of ('a * 'a * 'a ThingCollection)
   | ManyThings of ('a list * 'a ThingCollection)
   | Nothing
;

fun number_of_things (Nothing) = 0
  | number_of_things (OneThing x) = 
   1 + number_of_things (#2 x)
  | number_of_things (TwoThings x) = 
   2 + number_of_things (#3 x)
  | number_of_things (ManyThings x) = 
   (length (#1 x)) + number_of_things (#2 x)
;
   
(* Part 6 *)
(* Part 7 *)
(* Part 8 *)
(* Part 9 *)
(* Part 10 *)

