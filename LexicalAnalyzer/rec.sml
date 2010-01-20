(*
  Keywords: int bool fn write writeline if else while true false return var unit
  Assignment: :=
  Punctuation: { } ( ) , ; ->
  Logical Operators: & |
  Relational Operators: = < > ! = <= >=
  Arithmetic Binary Operators: + - * /
  Unary Operators: !
  Numbers: (0 ∪ ... ∪ 9)+
  Identifiers: (a ∪ ... ∪ z ∪ A ∪ ... ∪ Z) (a ∪ ... ∪ z ∪ A ∪ ... ∪ Z ∪ 0 ∪ ... ∪ 9)∗
*)

datatype 'a Token =
    Keywords of 'a
    | Assignment of 'a
    | Punctuation of 'a
    | Logical of 'a
    | Relational of 'a
    | ArithmeticBinary of 'a
    | Unary of 'a
    | Numbers of 'a
    | Identifiers of 'a
    | NONE
;

fun toTok ""          = false
  | toTok "int"       = true
  | toTok "bool"      = true
  | toTok "fn"        = true
  | toTok "write"     = true
  | toTok "writeline" = true
  | toTok "if"        = true
  | toTok "else"      = true
  | toTok "while"     = true
  | toTok "true"      = true
  | toTok "false"     = true
  | toTok "return"    = true
  | toTok "var"       = true
  | toTok "unit"      = true
  | toTok ":="        = true
  | toTok "{"         = true
  | toTok "}"         = true
  | toTok "("         = true
  | toTok ")"         = true
  | toTok ","         = true
  | toTok ";"         = true
  | toTok "->"        = true
  | toTok "&"         = true
  | toTok "|"         = true
  | toTok "="         = true
  | toTok ">"         = true
  | toTok "<"         = true
  | toTok "<="        = true
  | toTok ">="        = true
  | toTok "+"         = true
  | toTok "-"         = true
  | toTok "*"         = true
  | toTok "/"         = true
  | toTok "!"         = true
  | toTok str         = false
;

fun read_token instr = 
  let
    val x = lookahead instr;
  in
    if Char.isSpace x then
      NONE
    else
      if Char.isAlpha x then
        read_alpha instr
      else
        if Char.isDigit x then
          read_digit instr
        else
          read_symbol instr
        ;
      ;
    ;
  end
;
      
fun recognizeToken nil = ()
  | recognizeToken instr =
    if TextIO.endOfStream instr then 
      print "End Of File"
    else
      case (read_token instr) of
           (Keywords x) => print x
         | (Assignment x) => print x
         | (Punctuation x) => print x
         | (Logical x) => print x
         | (Relational x) => print x
         | (ArithmeticBinary x) => print x
         | (Unary x) => print x
         | (Numbers x) => print x
         | (Identifiers x) => print x
         | (NONE) => ()
      ;
;

