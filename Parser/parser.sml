(**
 * This is for hw3. It has 3 parts. 
 * Part 1: Parses code to verify that it is legal
 * Part 2: Builds AST
 * Part 3: Prints AST
 *)

(* Takes a file ptr from parse. *)
fun parse_ptr fptr =


(* Main func for Part 1 *)
fun parse filename = 
   parse_ptr (TextIO.openIn filename)
;

