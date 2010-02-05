
(*
use "lexer.sml";
val file = TextIO.openIn "tests/2_3_ast_echo/goodparse22.df";
nextToken file;
*)

use "parser.sml";
parse "tests/2_3_ast_echo/goodparse01.df";

