(**
 * HW 4 - Part 1 of the interpreter.
 * @author Nathaniel "Nat" Welch
 *)

(* import Keen's solution to hw3 *)
use "parser.sml";
open HashTable;
exception Oops;

(*
* how to use table...
* insert tbl ("foo", 9);
* lookup tbl "foo";
*)

fun eval_statement (ST_COMPOUND sl) st = (0, st)
  | eval_statement (ST_ASSIGN(str, x)) st = (0, st)
  | eval_statement (ST_WRITE(x)) st = (0, st)
  | eval_statement (ST_WRITELINE(x)) st = (0, st)
  | eval_statement (ST_IF(x, s1, s2)) st = (0, st)
  | eval_statement (ST_WHILE(x, s)) st = (0, st)
  | eval_statement (ST_RETURN(x)) st = (0, st)
;

fun eval_declarations [] st = (0, st)
  | eval_declarations ((DECL d)::ds) st = 
   eval_declarations ds (insert st (d, 0); st)

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
   let
     val hash_fn : string->word = HashString.hashString;
     val cmp_fn : string * string->bool = (op =);
     val initial_size : int = 101
     val tbl : (string, int) hash_table = mkTable (hash_fn, cmp_fn) (initial_size, Oops);
     val (v, s) = eval_program (parse filename) tbl;
   in
     v
   end
;

