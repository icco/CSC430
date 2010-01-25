(**
 * Part2 of lab 2. Requires code from part one in same directory.
 * @author Nathaniel "Nat" Welch
 *)

use "rec.sml";

datatype token =
  TK_AND
  | TK_ARROW
  | TK_ASSIGN
  | TK_BOOL
  | TK_COMMA
  | TK_DIVIDE
  | TK_EOF
  | TK_ELSE
  | TK_EQUALS
  | TK_FALSE
  | TK_GT
  | TK_GTE
  | TK_FN
  | TK_ID of string
  | TK_IF
  | TK_INT
  | TK_LBRACE
  | TK_LPAREN
  | TK_LT
  | TK_LTE
  | TK_MINUS
  | TK_NE
  | TK_NUM of int
  | TK_NOT
  | TK_OR
  | TK_PLUS
  | TK_RBRACE
  | TK_RETURN
  | TK_RPAREN
  | TK_SEMI
  | TK_TIMES
  | TK_TRUE
  | TK_UNIT
  | TK_VAR
  | TK_WHILE
  | TK_WRITELINE
  | TK_WRITE
  | TK_OTHER of string
  | NONE
;

(*
  Keywords: int bool fn write writeline if else while true false return var unit
*)
fun nextToken instr =
  case (read_token instr) of 
       Keyword "true" => TK_TRUE
     | Keyword "false" => TK_FALSE
     | Keyword "int" => TK_INT
     | Keyword "bool" => TK_BOOL
     | Keyword "fn" => TK_FN
     | Keyword "write" => TK_WRITE
     | Keyword "writeline" => TK_WRITELINE
     | Keyword "if" => TK_IF
     | Keyword "else" => TK_ELSE
     | Keyword "while" => TK_WHILE
     | Keyword "return" => TK_RETURN
     | Keyword "unit" => TK_UNIT
     | Keyword "var" => TK_VAR
     | Keyword x => TK_OTHER x
     | Assignment ":=" => TK_ASSIGN
     | Assignment x => TK_OTHER x
     | Punctuation ")" => TK_RPAREN
     | Punctuation "(" => TK_LPAREN
     | Punctuation "}" => TK_RBRACE
     | Punctuation "{" => TK_LBRACE
     | Punctuation "," => TK_COMMA
     | Punctuation ";" => TK_SEMI
     | Punctuation "->" => TK_ARROW
     | Punctuation x => TK_OTHER x
     | Logical "&" => TK_AND
     | Logical "|" => TK_OR
     | Logical x => TK_OTHER x
     | Relational "!=" => TK_NE
     | Relational ">=" => TK_GTE
     | Relational "<=" => TK_LTE
     | Relational "=" => TK_EQUALS
     | Relational ">" => TK_GT
     | Relational "<" => TK_LT
     | Relational x => TK_OTHER x
     | ArithmeticBinary "+" => TK_PLUS
     | ArithmeticBinary "*" => TK_TIMES
     | ArithmeticBinary "-" => TK_MINUS
     | ArithmeticBinary "/" => TK_DIVIDE
     | ArithmeticBinary x => TK_OTHER x
     | Unary "!" => TK_NOT
     | Unary x => TK_OTHER x
     | Number x => TK_NUM (valOf (Int.fromString x))
     | Identifier x => TK_ID x
     | Other x => TK_OTHER x
     | EOF => TK_EOF
     | None => NONE
;

