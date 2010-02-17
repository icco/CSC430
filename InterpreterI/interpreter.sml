(**
 * HW 4 - Part 1 of the interpreter.
 * @author Nathaniel "Nat" Welch
 *)

(* import Keen's solution to hw3 *)
use "parser.sml";
open HashTable;
exception Oops;

datatype ret =
   R_NUM of int
   | R_ID of string
   | R_TRUE
   | R_FALSE
   | R_UNIT
;

fun retToString (R_NUM x) = Int.toString x
  | retToString (R_ID x) = x
  | retToString R_TRUE = "true"
  | retToString R_FALSE = "false"
  | retToString R_UNIT = "unit"
;

fun update_table tbl (s:string, v) =
   (
     insert tbl (s, v);
     (v, tbl)
   )
;

fun eval_expression (EXP_ID x) sta = ((lookup sta x), sta)
  | eval_expression (EXP_NUM x) sta = ((R_NUM x), sta)
  | eval_expression (EXP_TRUE) sta = ((R_TRUE), sta)
  | eval_expression (EXP_FALSE) sta = ((R_FALSE), sta)
  | eval_expression (EXP_UNIT) sta = ((R_UNIT), sta)
  | eval_expression ex sta =
   (R_NUM 0, sta)
;

fun eval_write ex sta (nl:bool) =
   let
      val (v, s1) = eval_expression ex sta;
      val lineEnd = if nl then "\n" else " ";
   in
      (
         print ((retToString v) ^ lineEnd);
         (v, s1)
      )
   end
;

fun eval_assignment str ex sta =
   let
      val (v, s1) = eval_expression ex sta;
   in
      update_table s1 (str, v)
   end
;

fun eval_compound [] sta = (R_NUM 0, sta)
  | eval_compound (st::sl) sta =
   let
      val (v, s1) = eval_statement st sta;
   in
      eval_compound sl s1
   end
and eval_statement (ST_COMPOUND sl) st = eval_compound sl st
  | eval_statement (ST_ASSIGN(str, x)) st = eval_assignment str x st
  | eval_statement (ST_WRITE(x)) st = eval_write x st false
  | eval_statement (ST_WRITELINE(x)) st = eval_write x st true
  | eval_statement (ST_IF(x, s1, s2)) st = (R_NUM 0, st)
  | eval_statement (ST_WHILE(x, s)) st = (R_NUM 0, st)
  | eval_statement (ST_RETURN(x)) st = (R_NUM 0, st)
;

fun eval_declarations [] st = (R_NUM 0, st)
  | eval_declarations ((DECL d)::ds) st = 
   eval_declarations ds (insert st (d, R_NUM 0); st)
;

(* Don't do functions for this one. *)
fun eval_functions [] st = (R_NUM 0, st)
  | eval_functions (f::fs) st = 
    (R_NUM 0, st)
;

fun eval_program (PROGRAM(d, f, s)) st =
   let 
     val (v1, s1) = eval_declarations d st;
     val (v2, s2) = eval_functions f s1;
     val (v3, s3) = eval_statement s s1;
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
     val tbl : (string, ret) hash_table = mkTable (hash_fn, cmp_fn) (initial_size, Oops);
     val (v, s) = eval_program (parse filename) tbl;
   in
     ()
   end
;

