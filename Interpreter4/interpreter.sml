use "parser.sml";
use "map.sml";

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

fun insert_current chain id v = (insert (hd chain) id v; chain);
fun insert_state chain id v =
   let fun insert_state [] _ _ =
         raise UndefinedIdentifier
       | insert_state (env::envs) id v =
         if contains env id
         then insert env id v
         else insert_state envs id v
   in
      (insert_state chain id v; chain)
   end
;

fun lookup_state [] _ =
      raise UndefinedIdentifier
  | lookup_state (env::envs) id =
      if contains env id
      then lookup env id
      else lookup_state envs id
;

fun push_frame chain = (new_map ())::chain;

fun return_value (_, NONE) = Unit_Value
  | return_value (_, SOME v) = v
;

fun initial_value () = Unit_Value;

fun value_string (Int_Value n) =
      if n >= 0 then Int.toString n else "-" ^ (Int.toString (~n))
  | value_string (Bool_Value b) = Bool.toString b
  | value_string (Unit_Value) = "unit"
  | value_string (Func_Value _) = "<fun>"
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

fun evaluate_exp (EXP_ID id) chain = (lookup_state chain id
   handle UndefinedIdentifier => undeclared_identifier_error id)
  | evaluate_exp (EXP_NUM n) chain = Int_Value n
  | evaluate_exp EXP_TRUE chain = Bool_Value true
  | evaluate_exp EXP_FALSE chain = Bool_Value false
  | evaluate_exp EXP_UNIT chain = Unit_Value
  | evaluate_exp (EXP_INVOC (id, args)) chain =
      evaluate_invocation id args chain
  | evaluate_exp (EXP_BINARY (optr, lft, rht)) chain =
      apply_binary optr (evaluate_exp lft chain) (evaluate_exp rht chain)
  | evaluate_exp (EXP_UNARY (oper, opnd)) chain =
      apply_unary oper (evaluate_exp opnd chain)
  | evaluate_exp (EXP_ANON (ty, params, locals, body)) chain =
      Func_Value (ty, params, locals, body, chain)
and initialize_locals [] chain = chain
  | initialize_locals ((DECL (_, id))::locs) chain =
      initialize_locals locs (insert_current chain id (initial_value ()))
and initialize_formals _ _ _ [] _ =
      error_msg ("internal error: empty environment\n")
  | initialize_formals _ [] [] chain cur_chain = chain
  | initialize_formals fid [] _ _ _ =
      error_msg ("too many arguments in invocation of function '" ^ fid ^ "'\n")
  | initialize_formals fid _ [] _ _ =
      error_msg ("too few arguments in invocation of function '" ^ fid ^ "'\n")
  | initialize_formals fid ((DECL (ty, id))::forms) (act::acts) chain old_chain =
      initialize_formals
         fid forms acts
         (insert_current chain id (evaluate_exp act old_chain))
         old_chain
and evaluate_invocation id args cur_chain =
   let
      val func = lookup_state cur_chain id
         handle UndefinedIdentifier => undeclared_function_error id;
   in
      case func of
         Func_Value (ty, params, locals, body, stored_chain) =>
            return_value
               (evaluate_statement body
                  ((initialize_locals locals
                     (initialize_formals id params args
                        (push_frame stored_chain) cur_chain)), NONE))
       | _ =>
         error_msg ("attempt to invoke '" ^ type_string(func) ^ "' value as a function\n")

   end
and evaluate_statement _ (state as (_, SOME _)) =
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
and evaluate_conditional exp th el (state as (chain, _)) =
      let
         val guard = evaluate_exp exp chain
      in
         case guard of
            (Bool_Value true) => evaluate_statement th state
          | (Bool_Value false) => evaluate_statement el state
          | _ => if_type_error guard
      end
and evaluate_while exp body (state as (chain, _)) =
      let
         val guard = evaluate_exp exp chain
      in
         case guard of
            (Bool_Value true) => evaluate_statement (ST_WHILE (exp, body))
               (evaluate_statement body state)
          | (Bool_Value false) => state
          | _ => while_type_error guard
      end
and evaluate_assignment id exp (chain, rval) =
   ((lookup_state chain id
      handle UndefinedIdentifier => undeclared_identifier_error id);
      (insert_state chain id (evaluate_exp exp chain), rval))
and evaluate_print exp (state as (chain, _)) =
      (output (stdOut, (value_string (evaluate_exp exp chain)) ^ " "); state)
and evaluate_println exp (state as (chain, _)) =
      (output (stdOut, (value_string (evaluate_exp exp chain)) ^ "\n"); state)
and evaluate_return exp (state as (chain, _)) =
      (chain, SOME (evaluate_exp exp chain)) 
;

fun define_functions [] state = state
  | define_functions ((FUNCTION (ty, id, params, locals, body))::fs) chain =
      define_functions fs (insert_current chain id
         (Func_Value (ty, params, locals, body, chain)))
;

fun build_env [] gbl = [gbl]
  | build_env ((DECL (ty, id))::ds) gbl =
         build_env ds (insert gbl id (initial_value ()))

fun evaluate_program (PROGRAM (decls, funcs, body)) =
   evaluate_statement body
      (define_functions funcs (build_env decls (new_map ())), NONE)
;

use "type.sml";

fun interpret file =
   let
      val ast = parse file
   in (
      type_check ast;
      evaluate_program ast; 
      ()
   ) end
;

