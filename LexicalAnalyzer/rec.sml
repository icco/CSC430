(**
 * Token Recognizer for HW2
 * @author Nathaniel "Nat" Welch
 *)

(*
  Keywords: int bool fn write writeline if else while true false return var unit
  Assignment: :=
  Punctuation: { } ( ) , ; ->
  Logical Operators: & |
  Relational Operators: = < > != <= >=
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
    | Other of 'a
    | EOF
    | NONE
;

fun build_token "int"       = Keyword "int"       
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
  | build_token "!="        = Relational "!="  
  | build_token ">"         = Relational ">"  
  | build_token "<"         = Relational "<"  
  | build_token "<="        = Relational "<=" 
  | build_token ">="        = Relational ">=" 
  | build_token "+"         = ArithmeticBinary "+"
  | build_token "-"         = ArithmeticBinary "-"
  | build_token "*"         = ArithmeticBinary "*"
  | build_token "/"         = ArithmeticBinary "/"
  | build_token "!"         = Unary "!" 
  | build_token " "         = Other "space"
  | build_token "\n"        = Other "newline"
  | build_token "\t"        = Other "tab"
  | build_token str         = 
   let
     val x = (Int.fromString str)
   in
     if (isSome x) then
       Number str
     else
       Identifier str
   end
;

fun read_alpha instr str =
  let
    val x = (valOf (TextIO.lookahead instr));
  in
    if ((Char.isAlpha x) orelse (Char.isDigit x)) then
      read_alpha instr (str ^ (TextIO.inputN (instr, 1)))
    else
      build_token str
  end
;

fun read_digit instr str =
  let
    val x = (valOf (TextIO.lookahead instr));
  in
    if (Char.isDigit x) then
      read_digit instr (str ^ (TextIO.inputN (instr, 1)))
    else
      build_token str
  end
;

fun consume_white instr str = 
  if (Char.isSpace (valOf (TextIO.lookahead instr))) then
    consume_white instr (TextIO.inputN (instr, 1))
  else
    ""
;

fun equals_parse instr "=" str =
   case str of
        ":" => build_token (str ^ "=")
      | ">" => build_token (str ^ "=")
      | "<" => build_token (str ^ "=")
      | "!" => build_token (str ^ "=")
      | ""  => build_token (str ^ "=")
      | abc => build_token (str)
;

fun dash_parse instr str =
   let
     val x = (valOf (TextIO.lookahead instr));
   in
     if str = "-" andalso x = #">" then 
       build_token (str ^ TextIO.inputN (instr, 1))
     else
       build_token str
   end
;

fun bang_parse instr str =
   let
     val x = (valOf (TextIO.lookahead instr));
   in
     if str = "!" andalso x = #"=" then 
       build_token (str ^ TextIO.inputN (instr, 1))
     else
       build_token str
   end
;

fun left_arrow_parse instr str =
   let
     val x = (valOf (TextIO.lookahead instr));
   in
     if str = "<" andalso x = #"=" then 
       build_token (str ^ TextIO.inputN (instr, 1))
     else
       build_token str
   end
;

fun right_arrow_parse instr str =
   let
     val x = (valOf (TextIO.lookahead instr));
   in
     if str = ">" andalso x = #"=" then 
       build_token (str ^ TextIO.inputN (instr, 1))
     else
       build_token str
   end
;

fun read_symbol instr str =
  let
    val x = (valOf (TextIO.lookahead instr));
  in
    if ((Char.isAlpha x) orelse (Char.isDigit x) orelse (Char.isSpace x)) then
      build_token str
    else 
      (
      case x of
           #":" => read_symbol instr (str ^ TextIO.inputN (instr, 1))
         | #"{" => build_token (TextIO.inputN (instr, 1))
         | #"}" => build_token (TextIO.inputN (instr, 1))
         | #"(" => build_token (TextIO.inputN (instr, 1))
         | #")" => build_token (TextIO.inputN (instr, 1))
         | #"," => build_token (TextIO.inputN (instr, 1))
         | #";" => build_token (TextIO.inputN (instr, 1))
         | #"&" => build_token (TextIO.inputN (instr, 1))
         | #"|" => build_token (TextIO.inputN (instr, 1))
         | #"+" => build_token (TextIO.inputN (instr, 1))
         | #"*" => build_token (TextIO.inputN (instr, 1))
         | #"/" => build_token (TextIO.inputN (instr, 1))
         | #"-" => dash_parse instr ((TextIO.inputN (instr, 1)) ^ str)
         | #">" => right_arrow_parse instr ((TextIO.inputN (instr, 1)) ^ str)
         | #"!" => bang_parse instr ((TextIO.inputN (instr, 1)) ^ str)
         | #"<" => left_arrow_parse instr ((TextIO.inputN (instr, 1)) ^ str)
         | #"=" => equals_parse instr (TextIO.inputN (instr, 1)) str
         | strb => build_token (TextIO.inputN (instr, 1))
       )
  end
  ;

fun read_token instr = 
  if (TextIO.endOfStream instr) then 
    EOF
  else
    (
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
    )
;
      
fun recognizeToken instr =
    if (TextIO.endOfStream instr) then 
      print "end-of-file\n"
    else
      case (read_token instr) of
           (Keyword x) => print ("keyword: " ^ x ^ "\n")
         | (Assignment x) => print ("symbol: " ^ x ^ "\n")
         | (Punctuation x) => print ("symbol: " ^ x ^ "\n")
         | (Logical x) => print ("symbol: " ^ x ^ "\n")
         | (Relational x) => print ("symbol: " ^ x ^ "\n")
         | (ArithmeticBinary x) => print ("symbol: " ^ x ^ "\n")
         | (Unary x) => print ("symbol: " ^ x ^ "\n")
         | (Number x) =>  print ("number: " ^ x ^ "\n")
         | (Identifier x) => print ("identifier: " ^ x ^ "\n")
     (*  | (Other x) => print ("OTHER: " ^ x ^ "\n") *)
         | (Other x) => ()
         | (EOF) => print ("end-of-file\n")
         | (NONE) => ()
    ;
;
