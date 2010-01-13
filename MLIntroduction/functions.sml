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

(* needed for parts 5 - 10 *)
datatype 'a ThingCollection =
     OneThing of ('a * 'a ThingCollection)
   | TwoThings of ('a * 'a * 'a ThingCollection)
   | ManyThings of ('a list * 'a ThingCollection)
   | Nothing
;

(* Part 5 *)
fun number_of_things (Nothing) = 0
  | number_of_things (OneThing x) = 
   1 + number_of_things (#2 x)
  | number_of_things (TwoThings x) = 
   2 + number_of_things (#3 x)
  | number_of_things (ManyThings x) = 
   (length (#1 x)) + number_of_things (#2 x)
;
   
(* Part 6 *)
fun number_of_OneThing (Nothing) = 0
  | number_of_OneThing (OneThing x) = 
   1 + number_of_OneThing (#2 x)
  | number_of_OneThing (TwoThings x) = 
   0 + number_of_OneThing (#3 x)
  | number_of_OneThing (ManyThings x) = 
   0 + number_of_OneThing (#2 x)
;

(* Part 7 *)
fun number_of_XThing f (Nothing) = 0 
  | number_of_XThing f (OneThing x) = 
   if f x then
     1 + number_of_XThing f (#2 x)
   else
     0 + number_of_XThing f (#2 x)
  | number_of_XThing f (TwoThings x) = 
   if f x then
     1 + number_of_XThing f (#3 x)
   else
     0 + number_of_XThing f (#3 x)
  | number_of_XThing f (ManyThings x) = 
   if f x then
     1 + number_of_XThing f (#2 x)
   else
     0 + number_of_XThing f (#2 x)
;

(* Part 8 *)
(* Part 9 *)
(* Part 10 *)

