(**
 * This is for hw3. It has 3 parts. 
 * Part 1: Parses code to verify that it is legal
 * Part 2: Builds AST
 * Part 3: Prints AST
 *)

(* Bring in lexer to turn txt into tokens *)
use "lexer.sml";

(**
 * Token to String
 *)
fun t2s TK_TRUE = "true"
  | t2s TK_FALSE = "false"
  | t2s TK_INT = "int"
  | t2s TK_BOOL = "bool"
  | t2s TK_FN = "fn"
  | t2s TK_WRITE = "write"
  | t2s TK_WRITELINE = "writeline"
  | t2s TK_IF = "if"
  | t2s TK_ELSE = "else"
  | t2s TK_WHILE = "while"
  | t2s TK_RETURN = "return"
  | t2s TK_UNIT = "unit"
  | t2s TK_VAR = "var"
  | t2s TK_ASSIGN = ":="
  | t2s TK_LPAREN = "("
  | t2s TK_RPAREN = ")"
  | t2s TK_LBRACE = "{"
  | t2s TK_RBRACE = "}"
  | t2s TK_COMMA = ","
  | t2s TK_SEMI = ";"
  | t2s TK_ARROW = "->"
  | t2s TK_AND = "&"
  | t2s TK_OR = "|"
  | t2s TK_NE = "!="
  | t2s TK_GTE = ">="
  | t2s TK_LTE = "<="
  | t2s TK_EQUALS = "="
  | t2s TK_GT = ">"
  | t2s TK_LT = "<"
  | t2s TK_PLUS = "+"
  | t2s TK_MINUS = "-"
  | t2s TK_TIMES = "*"
  | t2s TK_DIVIDE = "/"
  | t2s TK_NOT = "!"
  | t2s (TK_NUM x) = (Int.toString x)
  | t2s (TK_ID x) = "ID: " ^ x
  | t2s TK_EOF = "eof"
  | t2s x = "UNKNOW"
;

(**
 * General expect function
 *)
fun expect fstr (TK_ID _) (TK_ID _) = (nextToken fstr)
  | expect fstr a b = 
   if ( a = b ) then 
      nextToken fstr
   else 
   (
      TextIO.output (TextIO.stdErr, "expected '" ^ (t2s a) ^ "' got '" ^ (t2s b) ^ "'\n"); 
      OS.Process.exit OS.Process.failure;
      NONE
   )
;

(** I guess I could have used this in expect, but whatever...  *)
fun isId (TK_ID _) = true
  | isId x = false
;

(****** Grammer Tree Parsing **************************************************)

(* Loop *)
fun do_loop fstr curTok =
  curTok
;

(* Multop *)

(* Relop *)

(* Return *)
fun do_return fstr curTok =
  curTok
;

(* Simple  *)

(* Term *)

(* Unary *)

(* Unaryop *)
fun do_unaryop fstr curTok =
   expect fstr TK_NOT curTok
;

(* Write *)
fun do_write fstr curTok =
  curTok
;

(* Addop *)

(* Arguments *)

(* Boolop *)

(* Boolterm *)

(* Expression *)

(* Factor *)

(* Assignment *)
fun do_assignment fstr curTok =
   expect fstr TK_SEMI (
      do_expression fstr (
         expect fstr TK_ASSIGN (
            expect fstr TK_ID curTok
         )
      )
   )
;

(* Conditional *)
fun do_else fstr curTok =
   if curTok = TK_ELSE then
     do_compound_statement fstr (expect fstr TK_ELSE curTok)    
   else
     curTok
;

fun do_conditional fstr curTok =
   do_else fstr (
     do_compound_statement fstr (
       expect fstr TK_RPAREN (
         do_expresssion fstr (
           expect fstr TK_LPAREN (
             expect fstr TK_IF curTok
           )
         )
       )
     )
   )
;

(* Compound statement : { statement* } *)
(* Statement: compound statement | assignment | write | conditional | loop | return *)
fun do_compound_statement fstr curTok =
  expect fstr TK_RBRACE (do_statement fstr (expect fstr TK_LBRACE curTok))
and do_statement fstr curTok =
   case curTok of
        (TK_ID x) => do_assignment fstr curTok
      | TK_WRITE => do_write fstr curTok
      | TK_IF => do_conditional fstr curTok
      | TK_WHILE => do_loop fstr curTok
      | TK_RETURN => do_return fstr curTok
      | TK_LBRACE => do_compound_statement fstr curTok
      | y => curTok
;

(* Declarations -> var id {, id}* ; * *)
(* TODO: Support more than one declaration*)
fun comma_id fstr curTok =
  if (curTok = TK_COMMA) then
    comma_id fstr (expect fstr (TK_ID "x") (expect fstr TK_COMMA curTok))
  else 
    curTok
;

fun do_declarations fstr curTok =
  if (curTok = TK_VAR) then
    expect fstr TK_SEMI 
      (comma_id fstr 
         (expect fstr (TK_ID "x") 
            (expect fstr TK_VAR curTok)
         )
      )
  else 
    curTok
;

(* Parameter *)
fun do_parameter fstr curTok =
   expect fstr (TK_ID "x") curTok
;

(* Parameters *)
fun do_parameters fstr curTok =
  if (isId curTok)  then
    do_parameters fstr (do_parameter fstr curTok)
  else
      (
      if curTok = TK_COMMA then
        do_parameter fstr (expect fstr TK_COMMA curTok)
      else 
        curTok
      )
;

(* Function *)
fun do_function fstr curTok =
  do_compound_statement fstr (
    do_declarations fstr (
      expect fstr TK_RPAREN (
        do_parameters fstr (
          expect fstr TK_LPAREN (
            expect fstr (TK_ID "x") (expect fstr TK_FN curTok)
          )
        )
      )
    )
  )
;

(* Functions *)
fun do_functions fstr curTok =
  if curTok = TK_FN then
    do_function fstr curTok
  else
    curTok
;

(* Program *)
fun do_program fptr =
  do_declarations fptr (nextToken fptr)
;

(* Takes a file ptr from parse. *)
fun parse_ptr fptr =
  if (TextIO.endOfStream fptr) then 
    TK_EOF
  else
    do_program fptr  
;

(* Main func for Part 1 *)
fun parse filename = 
  let
    val f = (TextIO.openIn filename);
  in
    expect f TK_EOF (parse_ptr f)
  end
;

