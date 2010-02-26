use "parser.sml";
use "map.sml";

datatype value =
     Int_Value of int
   | Bool_Value of bool
   | Unit_Value
   | Func_Value of (string, value) HashTable.hash_table * statement
   | Invalid_Value
;

fun type_string (Int_Value _) = "int"
  | type_string (Bool_Value _) = "bool"
  | type_string (Unit_Value) = "unit"
  | type_string (Invalid_Value) = "<invalid>"
;

fun undeclared_identifier_error id =
   (output (stdErr, "use of undeclared identifier '" ^ id ^ "'\n");
      OS.Process.exit OS.Process.failure
   )
;

fun if_type_error found =
   (output (stdErr, "boolean guard required for 'if' statement, found " ^
      (type_string found) ^ "\n");
    OS.Process.exit OS.Process.failure
   )
;

fun while_type_error found =
   (output (stdErr, "boolean guard required for 'while' statement, found " ^
      (type_string found) ^ "\n");
    OS.Process.exit OS.Process.failure
   )
;

fun negation_type_error found =
   (output (stdErr, "boolean operand required for operator '" ^
      (operator_string OP_NOT) ^ "', found " ^ (type_string found) ^ "\n");
      OS.Process.exit OS.Process.failure
   )
;

fun binary_type_error elft erht flft frht oper =
   (output (stdErr, "operator '" ^ (operator_string oper) ^ "' requires " ^
      (type_string elft) ^ " * " ^ (type_string erht) ^ ", found " ^
      (type_string flft) ^ " * " ^ (type_string frht) ^ "\n");
      OS.Process.exit OS.Process.failure
   )
;

fun initial_value () = Unit_Value;

fun value_string (Int_Value n) =
      if n >= 0 then Int.toString n else "-" ^ (Int.toString (~n))
  | value_string (Bool_Value b) = Bool.toString b
  | value_string (Unit_Value) = "unit"
  | value_string Invalid_Value = "<invalid>"
;

fun arithmetic_operator oper = member oper arithmetic_operators;
fun relational_operator oper = member oper relational_operators;
fun boolean_operator oper = member oper boolean_operators;

fun apply_binary OP_PLUS (Int_Value lft) (Int_Value rht) =
      Int_Value (lft + rht)
  | apply_binary OP_MINUS (Int_Value lft) (Int_Value rht) =
      Int_Value (lft - rht)
  | apply_binary OP_TIMES (Int_Value lft) (Int_Value rht) =
      Int_Value (lft * rht)
  | apply_binary OP_DIVIDE (Int_Value lft) (Int_Value rht) =
      if (rht = 0)
      then (output (stdErr, "divide by zero\n"); Invalid_Value)
      else Int_Value (lft div rht)
  | apply_binary OP_EQ (Int_Value lft) (Int_Value rht) =
      Bool_Value (lft = rht)
  | apply_binary OP_LT (Int_Value lft) (Int_Value rht) =
      Bool_Value (lft < rht)
  | apply_binary OP_GT (Int_Value lft) (Int_Value rht) =
      Bool_Value (lft > rht)
  | apply_binary OP_NE (Int_Value lft) (Int_Value rht) =
      Bool_Value (lft <> rht)
  | apply_binary OP_LE (Int_Value lft) (Int_Value rht) =
      Bool_Value (lft <= rht)
  | apply_binary OP_GE (Int_Value lft) (Int_Value rht) =
      Bool_Value (lft >= rht)
  | apply_binary OP_AND (Bool_Value lft) (Bool_Value rht) =
      Bool_Value (lft andalso rht)
  | apply_binary OP_OR (Bool_Value lft) (Bool_Value rht) =
      Bool_Value (lft orelse rht)
  | apply_binary oper lft rht =
      if arithmetic_operator oper
      then binary_type_error (Int_Value 0) (Int_Value 0) lft rht oper
      else if relational_operator oper
      then binary_type_error (Int_Value 0) (Int_Value 0) lft rht oper
      else if boolean_operator oper
      then binary_type_error (Bool_Value true) (Bool_Value true) lft rht oper
      else Invalid_Value
;

fun apply_unary OP_NOT (Bool_Value b) = Bool_Value (not b)
  | apply_unary OP_NOT opnd = negation_type_error opnd
  | apply_unary _ _ = Invalid_Value
;

fun evaluate_statement (ST_COMPOUND c) state =
      evaluate_compound c state
  | evaluate_statement (ST_ASSIGN (id, exp)) state =
      evaluate_assignment id exp state
  | evaluate_statement (ST_WRITE exp) state =
      evaluate_print exp state
  | evaluate_statement (ST_WRITELINE exp) state =
      evaluate_println exp state
  | evaluate_statement (ST_IF (exp, th, el)) state =
      evaluate_conditional exp th el state
  | evaluate_statement (ST_WHILE (exp, body)) state =
      evaluate_while exp body state
  | evaluate_statement (ST_RETURN _) state =
      (output (stdErr, "return not supported at this time\n"); state)
and evaluate_compound [] state = state
  | evaluate_compound (s::ss) state =
      evaluate_compound ss (evaluate_statement s state)
and evaluate_conditional exp th el state =
      let
         val guard = evaluate_exp exp state
      in
         case guard of
            (Bool_Value true) => evaluate_statement th state
          | (Bool_Value false) => evaluate_statement el state
          | _ => if_type_error guard
      end
and evaluate_while exp body state =
      let
         val guard = evaluate_exp exp state
      in
         case guard of
            (Bool_Value true) => evaluate_statement (ST_WHILE (exp, body))
               (evaluate_statement body state)
          | (Bool_Value false) => state
          | _ => while_type_error guard
      end
and evaluate_function (Func_Value(fstate, x)) args state = 
   (evaluate_statement x (merge_state fstate state); Invalid_Value)
  | evaluate_function x args state = Invalid_Value
and evaluate_exp (EXP_ID id) state = (lookup state id
   handle UndefinedIdentifier => undeclared_identifier_error id)
  | evaluate_exp (EXP_NUM n) state = Int_Value n
  | evaluate_exp EXP_TRUE state = Bool_Value true
  | evaluate_exp EXP_FALSE state = Bool_Value false
  | evaluate_exp EXP_UNIT state = Unit_Value
  | evaluate_exp (EXP_INVOC (id, args)) state =
   if contains state id then
      evaluate_function (lookup state id) args state
   else (
      output (stdErr, ("use of undeclared function '" ^ id ^ "'\n"));
      Invalid_Value
   )
  | evaluate_exp (EXP_BINARY (optr, lft, rht)) state =
      apply_binary optr (evaluate_exp lft state) (evaluate_exp rht state)
  | evaluate_exp (EXP_UNARY (oper, opnd)) state =
      apply_unary oper (evaluate_exp opnd state)
and evaluate_assignment id exp state =
   ((lookup state id
      handle UndefinedIdentifier => undeclared_identifier_error id);
      insert state id (evaluate_exp exp state))
and evaluate_print exp state =
      (output (stdOut, (value_string (evaluate_exp exp state)) ^ " "); state)
and evaluate_println exp state =
      (output (stdOut, (value_string (evaluate_exp exp state)) ^ "\n"); state)
;


fun build_state [] state = state
  | build_state ((DECL id)::ds) state =
   build_state ds (insert state id (initial_value ()))
;

(**
 * Builds function table
 *)
fun build_functions [] state = state
  | build_functions (f::fs) state =
   build_functions fs (build_function f state)
and build_function (FUNCTION (name, params, dec, inside)) state =
   insert state name (Func_Value (
      (build_state dec (
         build_state params (
            new_map ())
         )
      ), inside)
   )
;

fun evaluate_program (PROGRAM (decls, funcs, body)) =
   evaluate_statement body (
      build_functions funcs (
         build_state decls (new_map ())
      )
   )
;

fun interpret file =
   let
      val ast = parse file
   in
      (evaluate_program ast; ())
   end
;

