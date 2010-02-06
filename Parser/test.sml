
(*
use "lexer.sml";
val file = TextIO.openIn "tests/2_3_ast_echo/goodparse22.df";
nextToken file;
parse "tests/2_3_ast_echo/goodparse01.df";
*)

use "parser.sml";
parse "tests/1_parser/badparse47.df";

