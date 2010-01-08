(**
 * A set of functions for Assignment #1: ML Introduction.
 *
 * @author Nathaniel "Nat" Welch
 *)

fun number_of v [] = 0
  | number_of v (h::l) = 
  if h = v then
    1 + number_of v l
  else
    0 + number_of v l 
;
