(**
 * HW 4 - Part 1 of the interpreter.
 * @author Nathaniel "Nat" Welch
 *)

(* Bring in Keen's solution to hw3 *)
use "parser.sml";

datatype 'a state =
   STATE of declaration * 'a
   | NONE
;

(* make this a big pattern match function to pass off work. *)
fun eval_statement x st =
   (0, st)
;

fun eval_declarations [] st = (0, st)
  | eval_declarations (d::ds) st = 
   eval_declarations ds ((STATE(d, NONE))::st)

(* Don't do functions for this one. *)
fun eval_functions [] st = (0, st)
  | eval_functions (f::fs) st = 
    (0, st)
;

fun eval_program (PROGRAM(d, f, s)) st = 
   let 
     val (v1, s1) = eval_declarations d st;
     val (v2, s2) = eval_functions f s1;
     val (v3, s3) = eval_statement s s2;
   in
     (v3, s3)
   end
;

(**
 * Generic entry point...
 *)
fun interpret filename = 
   eval_program (parse filename) []
;

