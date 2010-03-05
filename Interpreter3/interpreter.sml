use "parser.sml";
use "map.sml";

datatype value =
     Int_Value of int
   | Bool_Value of bool
   | Unit_Value
   | Func_Value of (declaration list * declaration list * statement)
   | Invalid_Value
;

datatype clojure =
   C_Node of value
 | C_None
;

fun type_string (Int_Value _) = "int"
  | type_string (Bool_Value _) = "bool"
  | type_string (Unit_Value) = "unit"
  | type_string (Func_Value func) = "<fun>"
  | type_string (Invalid_Value) = "<invalid>"
;

fun error_msg msg =
   (output (stdErr, msg); OS.Process.exit OS.Process.failure)
;

fun undeclared_identifier_error id =
   error_msg ("use of undeclared identifier '" ^ id ^ "'\n")
;

fun undeclared_function_error id =
   error_msg ("use of undeclared function '" ^ id ^ "'\n")
;

fun if_type_error found =
   error_msg ("boolean guard required for 'if' statement, found " ^
      (type_string found) ^ "\n")
;

fun while_type_error found =
   error_msg ("boolean guard required for 'while' statement, found " ^
      (type_string found) ^ "\n")
;

fun negation_type_error found =
   error_msg ("boolean operand required for operator '" ^
      (operator_string OP_NOT) ^ "', found " ^ (type_string found) ^ "\n")
;

fun binary_type_error elft erht flft frht oper =
   error_msg ("operator '" ^ (operator_string oper) ^ "' requires " ^
      (type_string elft) ^ " * " ^ (type_string erht) ^ ", found " ^
      (type_string flft) ^ " * " ^ (type_string frht) ^ "\n")
;

fun insert_global (gbl, stk, rval) id v = (insert gbl id v, stk, rval);

fun insert_state (gbl, [], rval) id v =
      (insert gbl id v, [], rval)
  | insert_state (gbl, frame::stk, rval) id v =
      if contains frame id
      then (gbl, (insert frame id v)::stk, rval)
      else (insert gbl id v, frame::stk, rval)
;

fun insert_local (gbl, [], rval) id v =
      error_msg ("internal error: empty stack\n")
  | insert_local (gbl, frame::stk, rval) id v =
      (gbl, (insert frame id v)::stk, rval)
;

fun lookup_state (gbl, [], _) id =
      if contains gbl id
      then lookup gbl id
      else raise UndefinedIdentifier
  | lookup_state (gbl, frame::stk, _) id =
      if contains frame id
      then lookup frame id
      else if contains gbl id
      then lookup gbl id
      else raise UndefinedIdentifier
;
fun push_frame (gbl, stk, _) = (gbl, (new_map ())::stk, NONE);
fun pop_frame (gbl, stk, rval) = (gbl, tl stk, rval);
fun return_value (_, _, NONE) = Unit_Value
  | return_value (_, _, SOME v) = v
;

fun initial_value () = Unit_Value;

fun value_string (Int_Value n) =
      if n >= 0 then Int.toString n else "-" ^ (Int.toString (~n))
  | value_string (Bool_Value b) = Bool.toString b
  | value_string (Unit_Value) = "unit"
  | value_string (Func_Value _) = "<func>"
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

fun evaluate_exp (EXP_ID id) state = (lookup_state state id
   handle UndefinedIdentifier => undeclared_identifier_error id)
  | evaluate_exp (EXP_NUM n) state = Int_Value n
  | evaluate_exp EXP_TRUE state = Bool_Value true
  | evaluate_exp EXP_FALSE state = Bool_Value false
  | evaluate_exp EXP_UNIT state = Unit_Value
  | evaluate_exp (EXP_INVOC (id, args)) state =
      evaluate_invocation id args state
  | evaluate_exp (EXP_BINARY (optr, lft, rht)) state =
      apply_binary optr (evaluate_exp lft state) (evaluate_exp rht state)
  | evaluate_exp (EXP_UNARY (oper, opnd)) state =
      apply_unary oper (evaluate_exp opnd state)
  | evaluate_exp (EXP_FN (ps, ds, s)) state =
      Func_Value(ps, ds, s)
and initialize_locals [] state = state
  | initialize_locals ((DECL id)::locs) state =
      initialize_locals locs (insert_local state id (initial_value ()))
and initialize_formals _ [] [] state = state
  | initialize_formals fid [] _ state =
      error_msg ("too many arguments in invocation of function '" ^ fid ^ "'\n")
  | initialize_formals fid _ [] state =
      error_msg ("too few arguments in invocation of function '" ^ fid ^ "'\n")
  | initialize_formals fid ((DECL id)::forms) (act::acts)
   (state as (gbl, new::stk, _)) =
      initialize_formals fid forms acts (insert_local state id
         (evaluate_exp act (gbl, stk, NONE))) (* note using old stack *)
  | initialize_formals _ _ _ (state as (gbl, [], _)) =
      error_msg ("internal error: empty stack\n")
and evaluate_invocation id args state =
   let
      val func = lookup_state state id
         handle UndefinedIdentifier => undeclared_function_error id;
   in
      case func of
         Func_Value (params, locals, body) =>
            return_value
               (pop_frame (evaluate_statement body
                  (initialize_locals locals
                     (initialize_formals id params args (push_frame state)))))
       | _ =>
         error_msg ("attempt to invoke variable '" ^ id ^ "' as a function\n")
   end
and evaluate_statement _ (state as (_, _, SOME _)) =
      state
  | evaluate_statement (ST_COMPOUND c) state =
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
  | evaluate_statement (ST_RETURN exp) state =
      evaluate_return exp state
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
and evaluate_assignment id exp state =
   ((lookup_state state id
      handle UndefinedIdentifier => undeclared_identifier_error id);
      insert_state state id (evaluate_exp exp state))
and evaluate_print exp state =
      (output (stdOut, (value_string (evaluate_exp exp state)) ^ " "); state)
and evaluate_println exp state =
      (output (stdOut, (value_string (evaluate_exp exp state)) ^ "\n"); state)
and evaluate_return exp (state as (gbl, stk, _)) =
      (gbl, stk, SOME (evaluate_exp exp state)) 
;

fun define_functions [] state = state
  | define_functions ((FUNCTION (id, params, locals, body))::fs) state =
      define_functions fs (insert_global state id
         (Func_Value (params, locals, body)))
;

fun build_state [] gbl = (gbl, [], NONE)
  | build_state ((DECL id)::ds) gbl =
         build_state ds (insert gbl id (initial_value ()))
;

fun evaluate_program (PROGRAM (decls, funcs, body)) =
   evaluate_statement body
      (define_functions funcs (build_state decls (new_map ())))
;

fun interpret file =
   let
      val ast = parse file
   in
      (evaluate_program ast; ())
   end
;

