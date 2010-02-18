(**
 * HW 4 - Part 1 of the interpreter.
 * @author Nathaniel "Nat" Welch
 *
 * Note: Comments are kinda sparse. Sorry...
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

fun error msg =
   (print (msg^"\n"); OS.Process.exit OS.Process.failure; (R_UNIT))
;

fun retToString (R_NUM x) =
    let
       val v = Int.toString (Int.abs x);
    in
       if (((Int.sign x) > 0) orelse (x = 0)) then v else ("-" ^ v)
    end
  | retToString (R_ID x) = x
  | retToString R_TRUE = "true"
  | retToString R_FALSE = "false"
  | retToString R_UNIT = "unit"
;

fun typeOf (R_NUM x) = "int"
  | typeOf (R_ID x) = "string"
  | typeOf R_TRUE = "bool"
  | typeOf R_FALSE = "bool"
  | typeOf R_UNIT = "unit"
;

fun update_table tbl (s:string, v) =
   (
     insert tbl (s, v);
     (v, tbl)
   )
;

fun look_table tbl (s:string) =
   let
      val msg = ("use of undeclared identifier '" ^ s ^ "'");
   in
      lookup tbl s handle Oops => error msg
   end
;

fun eval_bool R_TRUE = true
  | eval_bool R_FALSE = false
  | eval_bool other = (raise Oops; false)
;

fun op2s OP_PLUS = "+"
  | op2s OP_MINUS = "-"
  | op2s OP_TIMES = "*"
  | op2s OP_DIVIDE = "/"
  | op2s OP_LT = "<"
  | op2s OP_GT = ">"
  | op2s OP_LE = "<="
  | op2s OP_GE = ">="
  | op2s OP_AND = "&"
  | op2s OP_OR = "|"
  | op2s OP_NE = "!="
  | op2s OP_EQ = "="
;

fun binary_err op_str (want1, want2) (found1, found2) =
   (
      "operator '" ^ (op_str) ^ "' requires " ^ want1 ^ " * " ^ want2
       ^ ", found " ^ found1 ^ " * " ^ found2
   )
;

fun eval_binary OP_PLUS (R_NUM x) (R_NUM y) = (R_NUM (x + y))
  | eval_binary OP_PLUS (x) (y) = (error (binary_err (op2s OP_PLUS) ("int", "int") ((typeOf x), (typeOf y))); (R_UNIT))
  | eval_binary OP_MINUS (R_NUM x) (R_NUM y) = (R_NUM (x - y))
  | eval_binary OP_MINUS (x) (y) = (error (binary_err (op2s OP_MINUS) ("int", "int") ((typeOf x), (typeOf y))); (R_UNIT))
  | eval_binary OP_TIMES (R_NUM x) (R_NUM y) = (R_NUM (x * y))
  | eval_binary OP_TIMES (x) (y) = (error (binary_err (op2s OP_TIMES) ("int", "int") ((typeOf x), (typeOf y))); (R_UNIT))
  | eval_binary OP_DIVIDE (R_NUM x) (R_NUM y) = (R_NUM (Int.div(x,  y)))
  | eval_binary OP_DIVIDE (x) (y) = (error (binary_err (op2s OP_DIVIDE) ("int", "int") ((typeOf x), (typeOf y))); (R_UNIT))
  | eval_binary OP_LT (R_NUM x) (R_NUM y) = if x < y then R_TRUE else R_FALSE
  | eval_binary OP_LT (x) (y) = (error (binary_err (op2s OP_LT) ("int", "int") ((typeOf x), (typeOf y))); (R_UNIT))
  | eval_binary OP_GT (R_NUM x) (R_NUM y) = if x > y then R_TRUE else R_FALSE
  | eval_binary OP_GT (x) (y) = (error (binary_err (op2s OP_GT) ("int", "int") ((typeOf x), (typeOf y))); (R_UNIT))
  | eval_binary OP_LE (R_NUM x) (R_NUM y) = if x <= y then R_TRUE else R_FALSE
  | eval_binary OP_LE (x) (y) = (error (binary_err (op2s OP_LE) ("int", "int") ((typeOf x), (typeOf y))); (R_UNIT))
  | eval_binary OP_GE (R_NUM x) (R_NUM y) = if x >= y then R_TRUE else R_FALSE
  | eval_binary OP_GE (x) (y) = (error (binary_err (op2s OP_GE) ("int", "int") ((typeOf x), (typeOf y))); (R_UNIT))
  | eval_binary OP_AND R_TRUE R_TRUE = R_TRUE
  | eval_binary OP_AND R_TRUE R_FALSE = R_FALSE
  | eval_binary OP_AND R_FALSE R_TRUE = R_FALSE
  | eval_binary OP_AND R_FALSE R_FALSE = R_FALSE
  | eval_binary OP_AND (x) (y) = (error (binary_err (op2s OP_AND) ("bool", "bool") ((typeOf x), (typeOf y))); (R_UNIT))
  | eval_binary OP_OR R_TRUE R_TRUE = R_TRUE
  | eval_binary OP_OR R_TRUE R_FALSE = R_TRUE
  | eval_binary OP_OR R_FALSE R_TRUE = R_TRUE
  | eval_binary OP_OR R_FALSE R_FALSE = R_FALSE
  | eval_binary OP_OR (x) (y) = (error (binary_err (op2s OP_OR) ("bool", "bool") ((typeOf x), (typeOf y))); (R_UNIT))
  | eval_binary OP_NE (R_NUM x) (R_NUM y) = if (not (x = y)) then R_TRUE else R_FALSE
  | eval_binary OP_NE (x) (y) = (error (binary_err (op2s OP_NE) ("int", "int") ((typeOf x), (typeOf y))); (R_UNIT))
  | eval_binary OP_EQ (R_NUM x) (R_NUM y) = if (x = y) then R_TRUE else R_FALSE
  | eval_binary OP_EQ (x) (y) = (error (binary_err (op2s OP_EQ) ("int", "int") ((typeOf x), (typeOf y))); (R_UNIT))
  | eval_binary opa (x) (y) = 
   let
      val msg = binary_err (op2s opa) ("some", "some") ((typeOf x), (typeOf y));
   in
      (error msg; (R_UNIT))
   end
;

fun eval_unary OP_NOT (R_TRUE) = (R_FALSE)
  | eval_unary OP_NOT (R_FALSE) = (R_TRUE)
  | eval_unary opa x = (
      error "boolean operand required for operator '!', found int"; 
      R_UNIT
   )
;

fun eval_expression (EXP_ID x) sta = ((look_table sta x), sta)
  | eval_expression (EXP_NUM x) sta = ((R_NUM x), sta)
  | eval_expression (EXP_TRUE) sta = ((R_TRUE), sta)
  | eval_expression (EXP_FALSE) sta = ((R_FALSE), sta)
  | eval_expression (EXP_UNIT) sta = ((R_UNIT), sta)
  | eval_expression (EXP_BINARY(opa, ex1, ex2)) sta =
   let
     val (v1, s1) = (eval_expression ex1 sta);
     val (v2, s2) = (eval_expression ex2 s1);
   in
     ((eval_binary opa v1 v2), s2)
   end
  | eval_expression (EXP_UNARY(opa, ex)) sta =
   let
     val (v1, s1) = (eval_expression ex sta);
   in
     ((eval_unary opa v1), s1)
   end
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
  | eval_statement (ST_IF(x, s1, s2)) st = 
   let
      val (v, st2) = eval_expression x st;
      val msg = ("boolean guard required for 'if' statement, found " ^ (typeOf v));
   in
      if (eval_bool v handle Oops => (error msg; false)) then
         (eval_statement s1 st2)
      else
         (eval_statement s2 st2)
   end
  | eval_statement (ST_WHILE(x, s)) st = eval_while x s st
  | eval_statement (ST_RETURN(x)) st = eval_expression x st
and eval_while x s st =
   let
      val (v2, st2) = eval_expression x st;
      val msg = ("boolean guard required for 'while' statement, found " ^ (typeOf v2));
   in
      if (eval_bool v2 handle Oops => (error msg; false)) then
         (let
            val (v3, st3) = eval_statement s st2;
         in
           (eval_while x s st3)
         end)
      else
         (v2, st2)
   end
;

fun eval_declarations [] st = (R_UNIT, st)
  | eval_declarations ((DECL d)::ds) st = 
   eval_declarations ds (insert st (d, R_UNIT); st)
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

