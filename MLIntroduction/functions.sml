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
  | weave x [] = 
   raise ImbalancedWeaving
  | weave (x::xs) (y::ys) =
   x::y::weave xs ys
;

(* Part 4 *)
open TextIO;
fun which_char x y z = if x = y then z else x;

fun filept_subst fstr [] [] = []
  | filept_subst fstr (x::xs) (y::ys) =
     (which_char (valOf (input1 fstr)) x y)::(filept_subst fstr xs ys)
;

fun file_subst file_name [] [] = []
  | file_subst file_name (x::xs) (y::ys) =
   filept_subst (openIn file_name)
;

(* Part 5 *)
datatype ’a ThingCollection =
     OneThing of (’a * ’a ThingCollection)
   | TwoThings of (’a * ’a * ’a ThingCollection)
   | ManyThings of (’a list * ’a ThingCollection)
   | Nothing
;

(* Part 6 *)
(* Part 7 *)
(* Part 8 *)
(* Part 9 *)
(* Part 10 *)

