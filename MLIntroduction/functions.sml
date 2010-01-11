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

(*
(* Part 3 *)
fun weave ([] []) = []
  | weave ((x::xs) (y::ys)) =
   []
;
*)

(* Part 4 *)
(* Part 5 *)
(* Part 6 *)
(* Part 7 *)
(* Part 8 *)
(* Part 9 *)
(* Part 10 *)

