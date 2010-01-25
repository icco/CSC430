use "rec.sml";

datatype Token =
    TK_LBRACE
    | TK_RBRACE
    | TK_RPAREN
    | TK_LPAREN
    | TK_IF
    | TK_ELSE
    | TK_LT
    | TK_TRUE
    | TK_FALSE
    | TK_SEMI
    | TK_WRITE
    | TK_WRITELINE
    | TK_NUM of int
    | TK_ID of string
    | TK_VAR
    | NONE
;

(*
* TK_AND : token
* TK_ARROW : token
* TK_ASSIGN : token
* TK_BOOL : token
* TK_COMMA : token
* TK_EOF : token
* TK_FALSE : token
* TK_FN : token
* TK_ID "abc456" : token
* TK_IF : token
* TK_INT : token
* TK_LBRACE : token
* TK_LPAREN : token
* TK_LT : token
* TK_NE : token
* TK_NUM 5 : token
* TK_PLUS : token
* TK_RBRACE : token
* TK_RETURN : token
* TK_RPAREN : token
* TK_SEMI : token
* TK_TIMES : token
* TK_TRUE : token
* TK_UNIT : token
* TK_VAR : token
* TK_WHILE : token
* TK_WRITELINE : token
* TK_WRITE : token
*)

fun nextToken instr =
  case (read_token instr) of 
       Keyword "true" => TK_TRUE
     | Keyword "false" => TK_FALSE
     | Keyword x => TK_LT
     | Assignment x => TK_VAR
     | Punctuation x => TK_VAR
     | Logical x => TK_VAR
     | Relational x => TK_VAR
     | ArithmeticBinary x => TK_VAR
     | Unary x => TK_VAR
     | Number x => TK_VAR
     | Identifier x => TK_ID x
     | Other x => TK_VAR
     | EOF => TK_VAR
     | None => NONE
;

