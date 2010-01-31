(**
 * A merge of hw2 into a Single file Lexer.
 * @author Nathaniel "Nat" Welch
 *)

datatype 'a NatToken =
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
    | None
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
        (
        (* Eat the white. *)
        TextIO.inputN (instr, 1);
        read_token instr
        )
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
 * Doing the work given a file pointer.
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

