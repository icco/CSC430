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
fun number_of_XThing fu x =  
  if (fu x) then
    case x of 
         Nothing          => 1 
       | OneThing(_,c)    => 1 + (number_of_XThing fu c) 
       | TwoThings(_,_,c) => 1 + (number_of_XThing fu c)
       | ManyThings(_,c)  => 1 + ((number_of_XThing fu c)) 
  else
    case x of 
         Nothing          => 0 
       | OneThing(_,c)    => 0 + (number_of_XThing fu c) 
       | TwoThings(_,_,c) => 0 + (number_of_XThing fu c)
       | ManyThings(_,c)  => 0 + ((number_of_XThing fu c)) 
;

(* Part 8 *)
fun number_of_TwoThings things = number_of_XThing (fn (TwoThings _) => true | _ => false) things;

(* Part 9 *)
fun map g l =
  foldr (fn (head, res) => (g head)::res) [] l
;

fun map_thing_collection fu (Nothing) = Nothing
  | map_thing_collection fu (OneThing x) =
   (OneThing((fu (#1 x)), (map_thing_collection fu (#2 x))))
  | map_thing_collection fu (TwoThings(h,j,c)) =
   (TwoThings((fu h), (fu j), (map_thing_collection fu c)))
  | map_thing_collection fu (ManyThings(l,c)) =
   (ManyThings((map fu l), (map_thing_collection fu c)))
;

(* Part 10 *)
(*
fun flatten_collection x =
    case x of 
         Nothing => Nothing
       | OneThing(z,c)    => ManyThings(z::[], (flatten_collection c))
       | TwoThings(h,j,c) => ManyThings(h::j::[], (flatten_collection c))
       | ManyThings(l,c)  => ManyThings(l@[], (flatten_collection c))
;
*)

fun flatten_things (Nothing) = []
  | flatten_things (OneThing(x,c)) = 
    x::(flatten_things c) 
  | flatten_things (TwoThings(x,y,c)) = 
    x::y::(flatten_things c) 
  | flatten_things (ManyThings(x,c)) = 
    x@(flatten_things c) 
;

fun flatten_collection x = ManyThings((flatten_things x), Nothing);
