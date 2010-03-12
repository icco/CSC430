open TextIO;

use "scanner.sml";
use "ast.sml";

fun err_expect want got =
   (output (stdErr,
      "expected '" ^ want ^ "', found '" ^ got ^"'\n");
      OS.Process.exit OS.Process.failure)
;

fun match_id fstr (TK_ID x) = (x, nextToken fstr)
  | match_id fstr tk = err_expect "identifier" (tkString tk)
;

fun match_num fstr (TK_NUM n) = (n, nextToken fstr)
  | match_num fstr tk = err_expect "number" (tkString tk)
;

fun match_tk fstr tk expected =
   if tk = expected
   then nextToken fstr
   else err_expect (tkString expected) (tkString tk)
;

fun match_eof fstr TK_EOF = TK_EOF
  | match_eof fstr tk = err_expect (tkString TK_EOF) (tkString tk)
;

fun typeToString (T_BOOL) = "bool "
  | typeToString (T_INT) = "int "
  | typeToString (T_UNIT) = "unit "
  | typeToString (T_FUNC ((ts), te)) =
   let
      val b = (List.foldr (op ^) "" (List.map (fn x => ((typeToString x) ^ "-> ")) ts));
      val c = (typeToString te);
   in
     (b ^ c)
   end
;

fun isPrimType TK_UNIT = true
  | isPrimType TK_INT = true
  | isPrimType TK_BOOL = true
  | isPrimType TK_ARROW = true
  | isPrimType _ = false
;

fun isStatement TK_LBRACE = true
  | isStatement (TK_ID _) = true
  | isStatement TK_WRITE = true
  | isStatement TK_WRITELINE = true
  | isStatement TK_IF = true
  | isStatement TK_WHILE = true
  | isStatement TK_RETURN = true
  | isStatement _ = false;

fun isExpression TK_LPAREN = true
  | isExpression (TK_ID _) = true
  | isExpression (TK_NUM _) = true
  | isExpression TK_TRUE = true
  | isExpression TK_FALSE = true
  | isExpression TK_UNIT = true
  | isExpression TK_NOT = true
  | isExpression TK_FN = true
  | isExpression _ = false;

fun isBoolop TK_AND = true
  | isBoolop TK_OR = true
  | isBoolop _ = false;

fun isRelop TK_EQ = true
  | isRelop TK_LT = true
  | isRelop TK_GT = true
  | isRelop TK_NE = true
  | isRelop TK_LE = true
  | isRelop TK_GE = true
  | isRelop _ = false;

fun isAddop TK_PLUS = true
  | isAddop TK_MINUS = true
  | isAddop _ = false;

fun isMultop TK_TIMES = true
  | isMultop TK_DIVIDE = true
  | isMultop _ = false;

fun isUnaryop TK_NOT = true
  | isUnaryop _ = false;

fun parse_repetition fstr tk pred parse_single =
   if pred tk
   then
      let
         val (x, tk1) = parse_single fstr tk;
         val (xs, tk2) = parse_repetition fstr tk1 pred parse_single
      in
         (x::xs, tk2)
      end
   else
      ([], tk)
;

fun parse_binary_operators fstr tk lft pred parse_operator parse_term =
   if pred tk
   then
      let
         val (oper, tk1) = parse_operator fstr tk;
         val (rht, tk2) = parse_term fstr tk1
      in
         parse_binary_operators fstr tk2
            (EXP_BINARY (oper, lft, rht)) pred parse_operator parse_term
      end
   else
      (lft, tk)
;

fun parse_separated_terms parse_sep parse_term fstr tk =
   let
      val tk1 = parse_sep fstr tk;
   in
      parse_term fstr tk1
   end
;

fun parse_type fstr (tk as (TK_INT)) =
   let
      val tk1 = match_tk fstr tk TK_INT;
   in
      (T_INT, tk1)
   end
  | parse_type fstr (tk as TK_UNIT) = 
   let
      val tk1 = match_tk fstr tk TK_UNIT;
   in
      (T_UNIT, tk1)
   end
  | parse_type fstr (tk as TK_BOOL) = 
   let
      val tk1 = match_tk fstr tk TK_BOOL;
   in
      (T_BOOL, tk1)
   end
  | parse_type fstr (tk as TK_LPAREN) =
   let
      val tk1 = match_tk fstr tk TK_LPAREN;
   in
     if isPrimType tk1 then (
       let
          val (t1, tk2) = parse_type fstr tk1;
          val (types, tk3) = parse_repetition fstr tk2 (fn tk => tk = TK_TIMES)
             (parse_separated_terms
                (fn fstr => fn tk => match_tk fstr tk TK_TIMES) parse_type);
          val tk4 = match_tk fstr tk3 TK_ARROW;
          val (t2, tk5) = parse_type fstr tk4;
          val tk6 = match_tk fstr tk5 TK_RPAREN;
       in
          ((T_FUNC ((t1::types), t2)), tk6)
       end
       ) else (
         let
            val tk4 = match_tk fstr tk1 TK_ARROW;
            val (t2, tk5) = parse_type fstr tk4;
            val tk6 = match_tk fstr tk5 TK_RPAREN;
         in
          ((T_FUNC ([], t2)), tk6)
         end
       )
   end
  | parse_type fstr tk =
   err_expect "type" (tkString tk)
;

fun parse_declarations fstr (tk as TK_VAR) =
   let
      val tk1 = match_tk fstr tk TK_VAR;
      val (ty, tkt) = parse_type fstr tk1;
      val (id, tk2) = match_id fstr tkt;
      val (decls, tk3) = parse_repetition fstr tk2 (fn tk => tk = TK_COMMA)
         (parse_separated_terms
            (fn fstr => fn tk => match_tk fstr tk TK_COMMA)
             (fn fstr => fn tk =>
               let
                  val (ty, tkt) = parse_type fstr tk;
                  val (id, tk1) = match_id fstr tkt;
               in
                  ((DECL (ty, id)), tk1)
               end
            )
          )
      val tk4 = match_tk fstr tk3 TK_SEMI;
      val (rest, tk5) = parse_declarations fstr tk4
   in
      (((DECL (ty, id))::decls)@rest, tk5)
   end
  | parse_declarations fstr tk = ([], tk)
;

(* expression parsing functions *)
fun parse_boolop fstr TK_AND =
   (OP_AND, match_tk fstr TK_AND TK_AND)
  | parse_boolop fstr TK_OR =
   (OP_OR, match_tk fstr TK_OR TK_OR)
  | parse_boolop fstr tk =
   err_expect ((tkString TK_AND) ^ " or " ^ (tkString TK_OR)) (tkString tk)
;

fun parse_relop fstr TK_EQ =
   (OP_EQ, match_tk fstr TK_EQ TK_EQ)
  | parse_relop fstr TK_LT =
   (OP_LT, match_tk fstr TK_LT TK_LT)
  | parse_relop fstr TK_GT =
   (OP_GT, match_tk fstr TK_GT TK_GT)
  | parse_relop fstr TK_NE =
   (OP_NE, match_tk fstr TK_NE TK_NE)
  | parse_relop fstr TK_LE =
   (OP_LE, match_tk fstr TK_LE TK_LE)
  | parse_relop fstr TK_GE =
   (OP_GE, match_tk fstr TK_GE TK_GE)
  | parse_relop fstr tk =
   err_expect
      ((tkString TK_EQ) ^ ", " ^ (tkString TK_LT) ^ ", " ^ (tkString TK_GT)
      ^ ", " ^ (tkString TK_NE) ^ ", " ^ (tkString TK_LE) ^ ", or "
      ^ (tkString TK_GE)) (tkString tk)
;
fun parse_addop fstr TK_PLUS =
   (OP_PLUS, match_tk fstr TK_PLUS TK_PLUS)
  | parse_addop fstr TK_MINUS =
   (OP_MINUS, match_tk fstr TK_MINUS TK_MINUS)
  | parse_addop fstr tk =
   err_expect ((tkString TK_PLUS) ^ " or " ^ (tkString TK_MINUS)) (tkString tk)
;

fun parse_multop fstr TK_TIMES =
   (OP_TIMES, match_tk fstr TK_TIMES TK_TIMES)
  | parse_multop fstr TK_DIVIDE =
   (OP_DIVIDE, match_tk fstr TK_DIVIDE TK_DIVIDE)
  | parse_multop fstr tk =
   err_expect
      ((tkString TK_TIMES) ^ " or " ^ (tkString TK_DIVIDE)) (tkString tk)
;

fun parse_unaryop fstr TK_NOT =
   (OP_NOT, match_tk fstr TK_NOT TK_NOT)
  | parse_unaryop fstr tk =
   err_expect (tkString TK_NOT) (tkString tk)
;

fun parse_parameters fstr (tk as (TK_ID _)) =
   let
      val (param, tk1) = parse_parameter fstr tk;
      val (params, tk2) = parse_repetition fstr tk1 (fn tk => tk = TK_COMMA)
         (parse_separated_terms
            (fn fstr => fn tk => match_tk fstr tk TK_COMMA) parse_parameter)
   in
      (param::params, tk2)
   end
  | parse_parameters fstr tk =
      ([], tk)
and parse_parameter fstr tk =
   let
      val (ty, tkt) = parse_type fstr tk;
      val (id, tk1) = match_id fstr tkt;
   in
      ((DECL (ty, id)), tk1)
   end
;

fun parse_expression fstr tk =
   let
      val (lft, tk1) = parse_boolterm fstr tk;
   in
      parse_binary_operators fstr tk1 lft isBoolop parse_boolop parse_boolterm
   end
and parse_boolterm fstr tk =
   let
      val (lft, tk1) = parse_simple fstr tk;
   in
      if isRelop tk1
      then
         let
            val (oper, tk2) = parse_relop fstr tk1;
            val (rht, tk3) = parse_simple fstr tk2
         in
            (EXP_BINARY (oper, lft, rht), tk3)
         end
      else
         (lft, tk1)
   end
and parse_simple fstr tk =
   let
      val (lft, tk1) = parse_term fstr tk;
   in
      parse_binary_operators fstr tk1 lft isAddop parse_addop parse_term
   end
and parse_term fstr tk =
   let
      val (lft, tk1) = parse_unary fstr tk;
   in
      parse_binary_operators fstr tk1 lft isMultop parse_multop parse_unary
   end
and parse_unary fstr tk =
   if isUnaryop tk
   then
      let
         val (oper, tk1) = parse_unaryop fstr tk;
         val (rht, tk2) = parse_factor fstr tk1
      in
         (EXP_UNARY (oper, rht), tk2)
      end
   else
      parse_factor fstr tk
and parse_factor fstr (tk as TK_LPAREN) =
   let
      val tk1 = match_tk fstr tk TK_LPAREN;
      val (exp, tk2) = parse_expression fstr tk1;
      val tk3 = match_tk fstr tk2 TK_RPAREN
   in
      (exp, tk3)
   end
  | parse_factor fstr (tk as (TK_ID id)) =
   let
      val (id, tk1) = match_id fstr tk;
   in
      if tk1 = TK_LPAREN
      then
         let
            val tk2 = match_tk fstr tk1 TK_LPAREN;
            val (args, tk3) = parse_arguments fstr tk2;
            val tk4 = match_tk fstr tk3 TK_RPAREN
         in
            (EXP_INVOC (id, args), tk4)
         end
      else
         (EXP_ID id, tk1)
   end
  | parse_factor fstr (tk as (TK_NUM n)) =
      (EXP_NUM n, #2(match_num fstr tk))
  | parse_factor fstr (tk as TK_TRUE) =
      (EXP_TRUE, match_tk fstr tk TK_TRUE)
  | parse_factor fstr (tk as TK_FALSE) =
      (EXP_FALSE, match_tk fstr tk TK_FALSE)
  | parse_factor fstr (tk as TK_UNIT) =
      (EXP_UNIT, match_tk fstr tk TK_UNIT)
  | parse_factor fstr (tk as TK_FN) =
   let
      val tk1 = match_tk fstr tk TK_FN;
      val (ty, tkt) = parse_type fstr tk1;
      val tk2 = match_tk fstr tkt TK_LPAREN;
      val (params, tk3) = parse_parameters fstr tk2;
      val tk4 = match_tk fstr tk3 TK_RPAREN;
      val (decls, tk5) = parse_declarations fstr tk4;
      val (body, tk6) = parse_compound fstr tk5;
   in
      (EXP_ANON (ty, params, decls, body), tk6)
   end
  | parse_factor fstr tk = err_expect "id or value" (tkString tk)
and parse_arguments fstr tk =
   if isExpression tk
   then
      let
         val (arg, tk1) = parse_expression fstr tk;
         val (args, tk2) = parse_repetition fstr tk1 (fn tk => tk = TK_COMMA)
            (parse_separated_terms
               (fn fstr => fn tk => match_tk fstr tk TK_COMMA)
               parse_expression)
      in
         (arg::args, tk2)
      end
   else
      ([], tk)
and parse_statement fstr (tk as TK_LBRACE) = parse_compound fstr tk
  | parse_statement fstr (tk as (TK_ID _)) = parse_assign fstr tk
  | parse_statement fstr (tk as TK_WRITE) = parse_write fstr tk
  | parse_statement fstr (tk as TK_WRITELINE) = parse_writeline fstr tk
  | parse_statement fstr (tk as TK_IF) = parse_conditional fstr tk
  | parse_statement fstr (tk as TK_WHILE) = parse_loop fstr tk
  | parse_statement fstr (tk as TK_RETURN) = parse_return fstr tk
  | parse_statement fstr other = err_expect "statement" (tkString other)
and parse_compound fstr (tk as TK_LBRACE) =
   let
      val tk1 = match_tk fstr tk TK_LBRACE;
      val (lst, tk2) = parse_repetition fstr tk1 isStatement parse_statement;
      val tk3 = match_tk fstr tk2 TK_RBRACE
   in
      (ST_COMPOUND lst, tk3)
   end
  | parse_compound fstr other = err_expect "{" (tkString other)
and parse_assign fstr tk =
   let
      val (id, tk1) = match_id fstr tk;
      val tk2 = match_tk fstr tk1 TK_ASSIGN;
      val (exp, tk3) = parse_expression fstr tk2;
      val tk4 = match_tk fstr tk3 TK_SEMI;
   in
      (ST_ASSIGN (id, exp), tk4)
   end
and parse_write fstr tk =
   let
      val tk1 = match_tk fstr tk TK_WRITE;
      val (exp, tk2) = parse_expression fstr tk1;
      val tk3 = match_tk fstr tk2 TK_SEMI;
   in
      (ST_WRITE exp, tk3)
   end
and parse_writeline fstr tk =
   let
      val tk1 = match_tk fstr tk TK_WRITELINE;
      val (exp, tk2) = parse_expression fstr tk1;
      val tk3 = match_tk fstr tk2 TK_SEMI;
   in
      (ST_WRITELINE exp, tk3)
   end
and parse_conditional fstr tk =
   let
      val tk1 = match_tk fstr tk TK_IF;
      val tk2 = match_tk fstr tk1 TK_LPAREN;
      val (exp, tk3) = parse_expression fstr tk2;
      val tk4 = match_tk fstr tk3 TK_RPAREN;
      val (t, tk5) = parse_compound fstr tk4;
      val (f, tk6) = parse_else fstr tk5
   in
      (ST_IF (exp, t, f), tk6)
   end
and parse_else fstr (tk as TK_ELSE) =
   parse_compound fstr (match_tk fstr tk TK_ELSE)
  | parse_else fstr tk = (ST_COMPOUND [], tk)
and parse_loop fstr tk =
   let
      val tk1 = match_tk fstr tk TK_WHILE;
      val tk2 = match_tk fstr tk1 TK_LPAREN;
      val (exp, tk3) = parse_expression fstr tk2;
      val tk4 = match_tk fstr tk3 TK_RPAREN;
      val (body, tk5) = parse_compound fstr tk4
   in
      (ST_WHILE (exp, body), tk5)
   end
and parse_return fstr tk =
   let
      val tk1 = match_tk fstr tk TK_RETURN;
      val (exp, tk2) = parse_expression fstr tk1;
      val tk3 = match_tk fstr tk2 TK_SEMI;
   in
      (ST_RETURN exp, tk3)
   end
;

(* function parsing *)
fun parse_functions fstr tk =
   let
   in
      parse_repetition fstr tk (fn tk => tk = TK_FN) parse_function
   end
and parse_function fstr tk =
   let
      val tk1 = match_tk fstr tk TK_FN;
      val (ty, tkt) = parse_type fstr tk1;
      val (id, tk2) = match_id fstr tkt;
      val tk3 = match_tk fstr tk2 TK_LPAREN;
      val (params, tk4) = parse_parameters fstr tk3;
      val tk5 = match_tk fstr tk4 TK_RPAREN;
      val (decls, tk6) = parse_declarations fstr tk5;
      val (body, tk7) = parse_compound fstr tk6;
   in
      (FUNCTION (ty, id, params, decls, body), tk7)
   end
;

fun parse_program fstr tk =
   let
      val (decls, tk1) = parse_declarations fstr tk;
      val (funcs, tk2) = parse_functions fstr tk1;
      val (body, tk3) = parse_statement fstr tk2
      val _ = match_eof fstr tk3
   in
      PROGRAM (decls, funcs, body)
   end
;

fun parse_stream fstr =
      parse_program fstr (nextToken fstr)
;

fun parse file =
   let
      val fstr = openIn(file)
         handle oops =>
            (output (stdErr, "cannot open file: " ^ file ^ "\n");
            OS.Process.exit OS.Process.failure)
   in
      parse_stream fstr
   end
;

(* printing below here *)
fun operator_string OP_PLUS = "+"
  | operator_string OP_MINUS = "-"
  | operator_string OP_TIMES = "*"
  | operator_string OP_DIVIDE = "/"
  | operator_string OP_EQ = "="
  | operator_string OP_LT = "<"
  | operator_string OP_GT = ">"
  | operator_string OP_NE = "!="
  | operator_string OP_LE = "<="
  | operator_string OP_GE = ">="
  | operator_string OP_NOT = "!"
  | operator_string OP_AND = "&"
  | operator_string OP_OR = "|"
;

fun print_decl (DECL (_, id)) =
   output (stdOut, id)
;

fun print_decls decls =
   map
      (fn d => (output (stdOut, "var "); print_decl d; output (stdOut, ";\n")))
      decls
;

fun print_parameters [] = ()
  | print_parameters (param::params) =
   (print_decl param;
      map (fn p => (output (stdOut, ", "); print_decl p)) params;
      ()
   )
;

fun print_type ty = (output (stdOut, (typeToString ty)));

fun print_expression (EXP_ID id) =
   output (stdOut, id)
  | print_expression (EXP_NUM n) =
   output (stdOut, Int.toString n)
  | print_expression EXP_TRUE =
   output (stdOut, "true")
  | print_expression EXP_FALSE =
   output (stdOut, "false")
  | print_expression EXP_UNIT =
   output (stdOut, "unit")
  | print_expression (EXP_INVOC (id, args))=
   (output (stdOut, id); output (stdOut, "("); print_arguments args;
      output (stdOut, ")"))
  | print_expression (EXP_BINARY (oper, lft, rht)) =
   (output (stdOut, "(");
   print_expression lft;
   output (stdOut, " ");
   output (stdOut, operator_string oper);
   output (stdOut, " ");
   print_expression rht;
   output (stdOut, ")")
   )
  | print_expression (EXP_UNARY (oper, rht)) =
   (output (stdOut, "(");
   output (stdOut, operator_string oper);
   print_expression rht;
   output (stdOut, ")")
   )
   | print_expression (EXP_ANON (ty, params, locals, body)) =
    (print_type ty;
    output (stdOut, "fn (");
    print_parameters params;
    output (stdOut, ")");
    print_decls locals;
    print_statement body
    )
and print_arguments [] = ()
  | print_arguments (arg::args) =
   (print_expression arg;
      map (fn a => (output (stdOut, ", "); print_expression a)) args;
      ()
   )
and print_statement (ST_COMPOUND sts) =
   (output (stdOut, "{\n"); map print_statement sts; output (stdOut, "}\n"))
  | print_statement (ST_ASSIGN (id, e)) =
   (output (stdOut, id ^ " := "); print_expression e; output (stdOut, ";\n"))
  | print_statement (ST_WRITE e) =
   (output (stdOut, "write "); print_expression e; output (stdOut, ";\n"))
  | print_statement (ST_WRITELINE e) =
   (output (stdOut, "writeline "); print_expression e; output (stdOut, ";\n"))
  | print_statement (ST_IF (g, t, e)) =
   (output (stdOut, "if ("); print_expression g; output (stdOut, ")\n");
      print_statement t; output (stdOut, "else\n"); print_statement e)
  | print_statement (ST_WHILE (g, b)) =
   (output (stdOut, "while ("); print_expression g; output (stdOut, ")\n");
      print_statement b)
  | print_statement (ST_RETURN e) =
   (output (stdOut, "return "); print_expression e; output (stdOut, ";\n"))
;

fun print_functions (FUNCTION (ty, id, params, locals, body)) =
   (output (stdOut, "fn " ^ id ^ "(");
   print_parameters params;
   output (stdOut, ")\n");
   print_decls locals;
   print_statement body
   )
;

fun print_funcs funcs =
   map print_functions funcs;

fun printAST (PROGRAM (decls, funcs, body)) =
   (print_decls decls; print_funcs funcs; print_statement body)
;
