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

fun isTok ""            = false
  | isTok "int"         = true
  | isTok "bool"        = true
  | isTok "fn"          = true
  | isTok "write"       = true
  | isTok "writeline"   = true
  | isTok "if"          = true
  | isTok "else"        = true
  | isTok "while"       = true
  | isTok "true"        = true
  | isTok "false"       = true
  | isTok "return"      = true
  | isTok "var"         = true
  | isTok "unit"        = true
  | isTok ":="          = true
  | isTok "{"           = true
  | isTok "}"           = true
  | isTok "("           = true
  | isTok ")"           = true
  | isTok ","           = true
  | isTok ";"           = true
  | isTok "->"          = true
  | isTok "&"           = true
  | isTok "|"           = true
  | isTok "="           = true
  | isTok ">"           = true
  | isTok "<"           = true
  | isTok "<="          = true
  | isTok ">="          = true
  | isTok "+"           = true
  | isTok "-"           = true
  | isTok "*"           = true
  | isTok "/"           = true
  | isTok "!"           = true
  | isTok str           = false
;
      

(*
fun recognizeToken nil = ()
  | recognizeToken instr =
    if TextIO.endOfStream instr then 
      print "End Of File"
    else
      case (read_token (inputN (instr, 1)) of
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
*)

