(**
 * A set of functions for Assignment #1: ML Introduction.
 *
 * @author Nathaniel "Nat" Welch
 *)

fun number_of v l = if (tl l) = [] then 1 else number_of v (tl l) 
  | number_of v nil = 0;
