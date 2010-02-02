(**
 * This is for hw3. It has 3 parts. 
 * Part 1: Parses code to verify that it is legal
 * Part 2: Builds AST
 * Part 3: Prints AST
 *)

(* Bring in lexer to turn txt into tokens *)
use "lexer.sml";

exception IncorrectSyntax of string;

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
  | t2s TK_LT = "<"
  | t2s TK_PLUS = "+"
  | t2s TK_MINUS = "-"
  | t2s TK_TIMES = "*"
  | t2s TK_DIVIDE = "/"
  | t2s TK_NOT = "!"
  | t2s (TK_NUM x) = (str x)
  | t2s (TK_ID x) = (str x)
  | t2s TK_EOF = "eof"
  | t2s x = "UNKNOW"
;

(* Ugh. Doing this this way will make the AST really hard. *)
fun expect fstr a b = 
   if ( a = b ) then 
     (nextToken fstr) 
   else 
   (
     TextIO.output (TextIO.stdErr, "Did not get wanted token\n"); 
     OS.Process.exit OS.Process.failure
   )
;

(****** Grammer Tree ******************************************************)
(* Addop *)

(* Arguments *)

(* Assignment *)

(* Boolop *)

(* Boolterm *)

(* Compound statement *)

(* Conditional *)

(* Declarations -> {var id {, id}* ; }* *)
(* TODO: Support more than one declaration*)
fun do_declarations fstr curTok =
  expect fstr TK_SEMI (expect fstr (TK_ID("x")) (expect fstr TK_VAR curTok))
;

(* Expression *)

(* Factor *)

(* Function *)

(* Functions *)

(* Loop *)

(* Multop *)

(* Parameter *)
(*
fun do_parameter fstr curTok =
   expect fstr TK_ID curTok
;
*)

(* Parameters *)

(* Relop *)

(* Return *)

(* Simple  *)

(* Statement *)

(* Term *)

(* Unary *)


(* Unaryop *)
(*
fun do_unaryop fstr curTok =
   expect fstr TK_NOT curTok
;
*)

(* Write *)

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
   parse_ptr (TextIO.openIn filename)
;
