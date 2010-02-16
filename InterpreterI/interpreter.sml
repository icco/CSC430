(**
 * HW 4 - Part 1 of the interpreter.
 * @author Nathaniel "Nat" Welch
 *)

(* Bring in Keen's solution to hw3 *)
use "parser.sml";


(**
 * Generic entry point...
 *)
fun interpret filename = 
   let 
     val ast = parse filename;
   in
     ast;
   end
;

