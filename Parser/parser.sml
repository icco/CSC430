(**
 * This is for hw3. It has 3 parts. 
 * Part 1: Parses code to verify that it is legal
 * Part 2: Builds AST
 * Part 3: Prints AST
 *)

(* Bring in lexer to turn txt into tokens *)
use "lexer.sml";

(* sends to the appropriate parser for the given token. *)
fun parse_tok fptr x = true;

(* Takes a file ptr from parse. *)
fun parse_ptr fptr =
   let
     val x = (parse_tok fptr (nextToken fptr));
   in
     if x then
       parse_ptr fptr
     else
       print "And here I thought you were going to give me working code.\n"
   end
;

(* Main func for Part 1 *)
fun parse filename = 
   parse_ptr (TextIO.openIn filename)
;

