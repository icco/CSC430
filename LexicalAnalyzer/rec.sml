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
    Keyword of 'a
    | Assignment of 'a
    | Punctuation of 'a
    | Logical of 'a
    | Relational of 'a
    | ArithmeticBinary of 'a
    | Unary of 'a
    | Number of 'a
    | Identifier of 'a
    | NONE
;

fun build_token ""          = NONE
  | build_token "int"       = Keyword "int"       
  | build_token "bool"      = Keyword "bool"      
  | build_token "fn"        = Keyword "fn"        
  | build_token "write"     = Keyword "write"     
  | build_token "writeline" = Keyword "writeline" 
  | build_token "if"        = Keyword "if"        
  | build_token "else"      = Keyword "else"      
  | build_token "while"     = Keyword "while"     
  | build_token "true"      = Keyword "true"      
  | build_token "false"     = Keyword "false"     
  | build_token "return"    = Keyword "return"    
  | build_token "var"       = Keyword "var"       
  | build_token "unit"      = Keyword "unit"      
  | build_token ":="        = Assignment ":="
  | build_token "{"         = Punctuation "{" 
  | build_token "}"         = Punctuation "}" 
  | build_token "("         = Punctuation "(" 
  | build_token ")"         = Punctuation ")" 
  | build_token ","         = Punctuation "," 
  | build_token ";"         = Punctuation ";" 
  | build_token "->"        = Punctuation "->" 
  | build_token "&"         = Logical "&" 
  | build_token "|"         = Logical "|" 
  | build_token "="         = Relational "="  
  | build_token ">"         = Relational ">"  
  | build_token "<"         = Relational "<"  
  | build_token "<="        = Relational "<=" 
  | build_token ">="        = Relational ">=" 
  | build_token "+"         = ArithmeticBinary "+"
  | build_token "-"         = ArithmeticBinary "-"
  | build_token "*"         = ArithmeticBinary "*"
  | build_token "/"         = ArithmeticBinary "/"
  | build_token "!"         = Unary "!" 
  | build_token " "         = NONE
  | build_token str         = NONE
;

fun read_alpha instr str =
  let
    val x = (valOf (TextIO.lookahead instr));
  in
    if ((Char.isAlpha x) orelse (Char.isDigit x)) then
      read_alpha instr (TextIO.inputN (instr, 1) ^ str)
    else
      build_token str
  end
;

fun read_digit instr str =
  let
    val x = (valOf (TextIO.lookahead instr));
  in
    if (Char.isDigit x) then
      read_digit instr (TextIO.inputN (instr, 1) ^ str)
    else
      build_token str
  end
;

fun read_symbol instr str =
  let
    val x = (valOf (TextIO.lookahead instr));
  in
    if ((Char.isAlpha x) orelse (Char.isDigit x)) then
      read_symbol instr (TextIO.inputN (instr, 1) ^ str)
    else
      build_token str
  end
;

fun read_token instr = 
  let
    val x = (valOf (TextIO.lookahead instr));
  in
    if Char.isSpace x then
      build_token (TextIO.inputN (instr, 1))
    else
      (
      if Char.isAlpha x then
        read_alpha instr ""
      else
        (
        if Char.isDigit x then
          read_digit instr ""
        else
          read_symbol instr ""
         )
      )
  end
;
      
(*
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
*)