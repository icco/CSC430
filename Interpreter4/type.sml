
fun die_death msg =
   (error_msg msg; T_ERROR)
;

fun binary_type_check_error elft erht flft frht oper =
   error_msg ("operator '" ^ (operator_string oper) ^ "' requires " ^
      (typeToString elft) ^ " * " ^ (typeToString erht) ^ ", found " ^
      (typeToString flft) ^ " * " ^ (typeToString frht) ^ "\n")
;

fun negation_type_check_error found =
   error_msg ("boolean operand required for operator '" ^
      (operator_string OP_NOT) ^ "', found " ^ (typeToString found) ^ "\n")
;


fun if_type_check_error found =
   error_msg ("boolean guard required for 'if' statement, found " ^
      (typeToString found) ^ "\n")
;

fun while_type_check_error found =
   error_msg ("boolean guard required for 'while' statement, found " ^
      (typeToString found) ^ "\n")
;
fun check_binary OP_PLUS   (T_INT) (T_INT) =
      T_INT
  | check_binary OP_MINUS  (T_INT) (T_INT) =
      T_INT
  | check_binary OP_TIMES  (T_INT) (T_INT) =
      T_INT
  | check_binary OP_DIVIDE (T_INT) (T_INT) =
      T_INT
  | check_binary OP_EQ     (T_INT) (T_INT) =
      T_BOOL
  | check_binary OP_LT     (T_INT) (T_INT) =
      T_BOOL
  | check_binary OP_GT     (T_INT) (T_INT) =
      T_BOOL
  | check_binary OP_NE     (T_INT) (T_INT) =
      T_BOOL
  | check_binary OP_LE     (T_INT) (T_INT) =
      T_BOOL
  | check_binary OP_GE     (T_INT) (T_INT) =
      T_BOOL
  | check_binary OP_AND    (T_BOOL) (T_BOOL) =
      T_BOOL
  | check_binary OP_OR     (T_BOOL) (T_BOOL) =
      T_BOOL
  | check_binary oper lft rht =
      if arithmetic_operator oper
      then binary_type_check_error (T_INT) (T_INT) lft rht oper
      else if relational_operator oper
      then binary_type_check_error (T_INT) (T_INT) lft rht oper
      else if boolean_operator oper
      then binary_type_check_error (T_BOOL) (T_BOOL) lft rht oper
      else die_death ""
;

fun apply_unary OP_NOT (T_BOOL) = T_BOOL
  | apply_unary OP_NOT opnd = negation_type_check_error opnd
  | apply_unary _ _ = T_ERROR
;

fun evaluate_exp (EXP_ID id) chain = (lookup_state chain id
   handle UndefinedIdentifier => undeclared_identifier_error id)
  | evaluate_exp (EXP_NUM n) chain = T_INT
  | evaluate_exp EXP_TRUE chain = T_BOOL
  | evaluate_exp EXP_FALSE chain = T_BOOL
  | evaluate_exp EXP_UNIT chain = T_UNIT
  | evaluate_exp (EXP_INVOC (id, args)) chain =
    (*  evaluate_invocation id args chain *)
    die_death "functions not supported"
  | evaluate_exp (EXP_BINARY (optr, lft, rht)) chain =
      check_binary optr (evaluate_exp lft chain) (evaluate_exp rht chain)
  | evaluate_exp (EXP_UNARY (oper, opnd)) chain =
      apply_unary oper (evaluate_exp opnd chain)
  | evaluate_exp (EXP_ANON (ty, params, locals, body)) chain =
      (*Func_Value (ty, params, locals, body, chain) *)
      die_death "anon functions not supported"
and initialize_locals [] chain = chain
  | initialize_locals ((DECL (ty, id))::locs) chain =
      initialize_locals locs (insert_current chain id ty)
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
            (T_BOOL) => evaluate_statement th state
 (*         | (T_BOOL) => evaluate_statement el state *)
          | _ => if_type_check_error guard
      end
and evaluate_while exp body (state as (chain, _)) =
      let
         val guard = evaluate_exp exp chain
      in
         case guard of
              (*
            (Bool_Value true) => evaluate_statement (ST_WHILE (exp, body))
               (evaluate_statement body state)
               *)
            (T_BOOL) => state
          | _ => while_type_check_error guard
      end
and evaluate_assignment id exp (chain, rval) =
   ((lookup_state chain id
      handle UndefinedIdentifier => undeclared_identifier_error id);
      (insert_state chain id (evaluate_exp exp chain), rval))
and evaluate_print exp (state as (chain, _)) =
      (state)
and evaluate_println exp (state as (chain, _)) =
      (state)
and evaluate_return exp (state as (chain, _)) =
      (chain, SOME (evaluate_exp exp chain)) 
;

fun define_functions [] state = state
  | define_functions ((FUNCTION (ty, id, params, locals, body))::fs) chain =
      define_functions fs (insert_current chain id ty)
        (* (Func_Value (ty, params, locals, body, chain))) *)
;

fun build_env [] gbl = [gbl]
  | build_env ((DECL (ty, id))::ds) gbl =
         build_env ds (insert gbl id (ty))

fun evaluate_program (PROGRAM (decls, funcs, body)) =
   evaluate_statement body
      (*(define_functions funcs (build_env decls (new_map ())), NONE)*)
      ((build_env decls (new_map ())), NONE)
;

fun interpret file =
   let
      val ast = parse file
   in (
      type_check ast;
      evaluate_program ast;
      ()
   ) end
;

