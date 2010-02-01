(**
 * This is for hw3. It has 3 parts. 
 * Part 1: Parses code to verify that it is legal
 * Part 2: Builds AST
 * Part 3: Prints AST
 *)

(* Bring in lexer to turn txt into tokens *)
use "lexer.sml";

exception IncorrectSyntax of string;

(**
 * My attempt to make this rediculously abstract. See if we have it so we can
 * pass in a list of terminals to check for, we can be uber simple and readable
 * it is possible though that this will fail miserably.
 *)
(*
fun terminal a b = 
   if ( a = b ) then 
     a 
   else 
   (
     TextIO.output (TextIO.stdErr, "Did not get wanted token"); 
     OS.Process.exit OS.Process.failure
   )
;

fun non_terminal [] curTok fstr = curTok
  | non_terminal x::xs curTok fstr =
   if ((terminal curTok x) = curTok) then
     non_terminal xs (nextToken fstr) fstr
   else
   (
     TextIO.output (TextIO.stdErr, "Did not get wanted token in a terminal"); 
     OS.Process.exit OS.Process.failure
   )
;

fun loop_fn f curTok fstr =
  if (f fstr curTok) then
    curTok
  else
    loop_fn f (nextToken fstr) fstr
;
*)

(* Ugh. Doing this this way will make the AST really hard. *)
fun expect fstr a b = 
   if ( a = b ) then 
     (nextToken fstr) 
   else 
   (
     TextIO.output (TextIO.stdErr, "Did not get wanted token"); 
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
  expect fstr TK_SEMI (expect fstr TK_ID (expect fstr TK_VAR curTok))
;

(* Expression *)

(* Factor *)

(* Function *)

(* Functions *)

(* Loop *)

(* Multop *)

(* Parameter *)
fun do_parameter fstr curTok =
   expect fstr TK_ID curTok
;

(* Parameters *)

(* Relop *)

(* Return *)

(* Simple  *)

(* Statement *)

(* Term *)

(* Unary *)


(* Unaryop *)
fun do_unaryop fstr curTok =
   expect fstr TK_NOT curTok
;

(* Write *)

(* Program *)

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

