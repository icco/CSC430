
fun die_death msg =
   (error_msg msg; T_ERROR)
;

fun binary_type_check_error elft erht flft frht oper =
   die_death ("operator '" ^ (operator_string oper) ^ "' requires " ^
      (typeToString elft) ^ " * " ^ (typeToString erht) ^ ", found " ^
      (typeToString flft) ^ " * " ^ (typeToString frht) ^ "\n")
;

fun negation_type_check_error found =
   error_msg ("boolean operand required for operator '" ^
      (operator_string OP_NOT) ^ "', found " ^ (typeToString found) ^ "\n")
;

fun type_assignment_error b a =
  error_msg ("type mismatch in assignment, cannot assign '" ^ (typeToString a) ^
  "' to '" ^ (typeToString b) ^ "'\n")
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

fun type_ap_unary OP_NOT (T_BOOL) = T_BOOL
  | type_ap_unary OP_NOT opnd = negation_type_check_error opnd
  | type_ap_unary _ _ = T_ERROR
;

fun check_exp (EXP_ID id) chain = (lookup_state chain id
   handle UndefinedIdentifier => undeclared_identifier_error id)
  | check_exp (EXP_NUM n) chain = T_INT
  | check_exp EXP_TRUE chain = T_BOOL
  | check_exp EXP_FALSE chain = T_BOOL
  | check_exp EXP_UNIT chain = T_UNIT
  | check_exp (EXP_INVOC (id, args)) chain =
    (*  check_invocation id args chain *)
    die_death "functions not supported"
  | check_exp (EXP_BINARY (optr, lft, rht)) chain =
      check_binary optr (check_exp lft chain) (check_exp rht chain)
  | check_exp (EXP_UNARY (oper, opnd)) chain =
      type_ap_unary oper (check_exp opnd chain)
  | check_exp (EXP_ANON (ty, params, locals, body)) chain =
      (*Func_Value (ty, params, locals, body, chain) *)
      die_death "anon functions not supported"
and init_type_locals [] chain = chain
  | init_type_locals ((DECL (ty, id))::locs) chain =
      init_type_locals locs (insert_current chain id ty)
and init_type_formals _ _ _ [] _ =
      error_msg ("internal error: empty environment\n")
  | init_type_formals _ [] [] chain cur_chain = chain
  | init_type_formals fid [] _ _ _ =
      error_msg ("too many arguments in invocation of function '" ^ fid ^ "'\n")
  | init_type_formals fid _ [] _ _ =
      error_msg ("too few arguments in invocation of function '" ^ fid ^ "'\n")
  | init_type_formals fid ((DECL (ty, id))::forms) (act::acts) chain old_chain =
      init_type_formals
         fid forms acts
         (insert_current chain id (check_exp act old_chain))
         old_chain
and check_invocation id args cur_chain =
(*
   let
      val func = lookup_state cur_chain id
         handle UndefinedIdentifier => undeclared_function_error id;
   in
      case func of
         Func_Value (ty, params, locals, body, stored_chain) =>
            return_value
               (check_statement body
                  ((init_type_locals locals
                     (init_type_formals id params args
                        (push_frame stored_chain) cur_chain)), NONE))
       | _ =>
         error_msg ("attempt to invoke '" ^ type_string(func) ^ "' value as a function\n")

   end
   *) T_ERROR
and check_statement _ (state as (_, SOME _)) =
      state
  | check_statement (ST_COMPOUND c) state =
      check_compound c state
  | check_statement (ST_ASSIGN (id, exp)) state =
      check_assignment id exp state
  | check_statement (ST_WRITE exp) state =
      check_print exp state
  | check_statement (ST_WRITELINE exp) state =
      check_println exp state
  | check_statement (ST_IF (exp, th, el)) state =
      check_conditional exp th el state
  | check_statement (ST_WHILE (exp, body)) state =
      check_while exp body state
  | check_statement (ST_RETURN exp) state =
      check_return exp state
and check_compound [] state = state
  | check_compound (s::ss) state =
      check_compound ss (check_statement s state)
and check_conditional exp th el (state as (chain, _)) =
      let
         val guard = check_exp exp chain
      in
         case guard of
            (T_BOOL) => ((check_statement th state; check_statement el state))
          | _ => if_type_check_error guard
      end
and check_while exp body (state as (chain, _)) =
      let
         val guard = check_exp exp chain
      in
         case guard of
              (*
            (Bool_Value true) => check_statement (ST_WHILE (exp, body))
               (check_statement body state)
               *)
            (T_BOOL) => (check_statement body state)
          | _ => while_type_check_error guard
      end
and check_assignment id exp (chain, rval) =
   let
      val ty = (lookup_state chain id handle UndefinedIdentifier => undeclared_identifier_error id);
      val nty = (check_exp exp chain);
   in
     if ty = nty then
        (insert_state chain id ty, rval)
     else
         type_assignment_error ty nty
   end
and check_print exp (state as (chain, _)) =
      (state)
and check_println exp (state as (chain, _)) =
      (state)
and check_return exp (state as (chain, _)) =
      (chain, SOME (check_exp exp chain)) 
;

fun define_functions [] state = state
  | define_functions ((FUNCTION (ty, id, params, locals, body))::fs) chain =
      define_functions fs (insert_current chain id ty)
        (* (Func_Value (ty, params, locals, body, chain))) *)
;

fun build_env [] gbl = [gbl]
  | build_env ((DECL (ty, id))::ds) gbl =
         build_env ds (insert gbl id (ty))

fun check_program (PROGRAM (decls, funcs, body)) =
   check_statement body
      (*(define_functions funcs (build_env decls (new_map ())), NONE)*)
      ((build_env decls (new_map ())), NONE)
;

fun type_check ast =
   check_program ast
;

