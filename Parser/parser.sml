(**
 * This is for hw3. It has 3 parts. 
 * Part 1: Parses code to verify that it is legal
 * Part 2: Builds AST
 * Part 3: Prints AST
 *)

(* Bring in lexer to turn txt into tokens *)
use "lexer.sml";

exception IncorrectSyntax of string;

(* Addop *)
(* Arguments *) 
(* Assignment *)
(* Boolop *)
(* Boolterm *)
(* Compound statement *)
(* Conditional *)
(* Declarations *)
fun do_declarations fptr TK_VAR = do_declarations fptr (nextToken fptr)
  | do_declarations fptr (TK_ID x) = 
   case (nextToken fptr) of 
        TK_COMMA => do_declaractions fptr (nextToken fptr)
      | TK_SEMI => fptr 
      | y = raise IncorrectSyntax("Incorrect Declaration\n")
;


(* Expression *)
(* Factor *)
(* Function *)
fun do_function fptr TK_FN = do_function fptr (nextToken fptr)
  | do_function fptr (TK_ID x) = do_function fptr (nextToken fptr)
  | do_function fptr TK_LPAREN = do_parameters fptr (nextToken fptr)
;

(* Functions *)
fun do_functions fptr x = 
  do_function fptr x
;
(* Loop *)
(* Multop *)
(* Parameter *)
(* Parameters *)
(* Relop *)
(* Return *)
(* Simple  *)
(* Statement *)
(* Term *)
(* Unary *)
(* Unaryop *)
(* Write *)
(* Program *) 
fun do_program fptr =
   do_statement (do_functions (do_declarations fptr))
;

(* sends to the appropriate parser for the given token. *)
fun parse_tok fptr TK_VAR = parse_declaration fptr
  | parse_tok fptr x = true;

(* Takes a file ptr from parse. *)
fun parse_ptr fptr =
  if (TextIO.endOfStream fptr) then 
    ()
  else
    do_program fptr  
;

(* Main func for Part 1 *)
fun parse filename = 
   parse_ptr (TextIO.openIn filename)
;

