open TextIO;

datatype token =
   TK_INT
   | TK_BOOL
   | TK_FN
   | TK_WRITE
   | TK_WRITELINE
   | TK_IF
   | TK_ELSE
   | TK_WHILE
   | TK_TRUE
   | TK_FALSE
   | TK_RETURN
   | TK_VAR
   | TK_UNIT
   | TK_NOT
   | TK_ASSIGN
   | TK_LBRACE
   | TK_RBRACE
   | TK_LPAREN
   | TK_RPAREN
   | TK_COMMA
   | TK_SEMI
   | TK_ARROW
   | TK_AND
   | TK_OR
   | TK_EQ
   | TK_LT
   | TK_GT
   | TK_NE
   | TK_LE
   | TK_GE
   | TK_PLUS
   | TK_MINUS
   | TK_TIMES
   | TK_DIVIDE
   | TK_NUM of int
   | TK_ID of string
   | TK_EOF
   | TK_NONE
;

val keywords = ["int", "bool", "fn", "write", "writeline",
   "if", "else", "while", "true", "false", "return", "var", "unit"];
val keywordTokens = [("int", TK_INT), ("bool", TK_BOOL),
   ("fn", TK_FN), ("write", TK_WRITE),
   ("writeline", TK_WRITELINE), ("if", TK_IF), ("else", TK_ELSE),
   ("while", TK_WHILE), ("true", TK_TRUE), ("false", TK_FALSE),
   ("return", TK_RETURN), ("var", TK_VAR), ("unit", TK_UNIT)];
val symbolTokens = [(":=", TK_ASSIGN), ("{", TK_LBRACE), ("}", TK_RBRACE),
   ("(", TK_LPAREN), (")", TK_RPAREN), (",", TK_COMMA), (";", TK_SEMI),
   ("->", TK_ARROW), ("=", TK_EQ), ("<", TK_LT), (">", TK_GT),
   ("!=", TK_NE), ("<=", TK_LE), (">=", TK_GE), ("+", TK_PLUS),
   ("-", TK_MINUS), ("*", TK_TIMES), ("/", TK_DIVIDE), ("&", TK_AND),
   ("|", TK_OR), ("!", TK_NOT)];

fun member a [] = false
  | member a (x::xs) = if a = x then true else member a xs
;

fun lookupToken s [] = TK_NONE
  | lookupToken s ((st, tk)::tks) =
      if s = st
      then tk
      else lookupToken s tks
;

fun clearWhitespace fstr =
   case lookahead fstr of
      SOME c => if Char.isSpace c
         then (input1 fstr; clearWhitespace fstr)
         else fstr
      | NONE => fstr
;

fun buildToken fstr pred s =
   if endOfStream fstr
      then s
      else case lookahead fstr of
         SOME c => if pred c
            then (input1 fstr; buildToken fstr pred (s ^ (str c)))
            else s
         | NONE => s
;

fun isKeyword s = member s keywords;

fun recognizeIdentifier fstr =
   let
      val id = buildToken fstr
         (fn x => (Char.isAlpha x) orelse (Char.isDigit x)) ""
   in 
      if isKeyword id
         then lookupToken id keywordTokens
         else TK_ID(id)
   end
;

fun recognizeNumber fstr =
   let
      val n = buildToken fstr Char.isDigit ""
      val num = Int.fromString(n)
   in 
      if isSome(num)
      then TK_NUM(valOf(num))
      else (output (stdErr, "invalid number: " ^ n ^ "\n");
         OS.Process.exit OS.Process.failure)
   end
;

fun symbol s =
   lookupToken s symbolTokens
;

fun buildSymbol fstr s need optional =
   let
      val input = lookahead fstr
   in
      if isSome(input) andalso (member (valOf input) need)
      then (input1 fstr; symbol (s ^ str(valOf(input))))
      else if optional
         then symbol s
         else (output (stdErr, "invalid symbol: " ^ s ^ "\n");
            OS.Process.exit OS.Process.failure)
   end
;

fun recognizeSymbol fstr =
   let
      val input = input1 fstr
   in
      case input of
         SOME c =>
         (
            case c of
                 #"=" => symbol (str c)
               | #"{" => symbol (str c)
               | #"}" => symbol (str c)
               | #"(" => symbol (str c)
               | #")" => symbol (str c)
               | #"," => symbol (str c)
               | #";" => symbol (str c)
               | #":" => buildSymbol fstr (str c) [#"="] false
               | #"<" => buildSymbol fstr (str c) [#"="] true
               | #">" => buildSymbol fstr (str c) [#"="] true
               | #"!" => buildSymbol fstr (str c) [#"="] true
               | #"+" => symbol (str c)
               | #"-" => buildSymbol fstr (str c) [#">"] true
               | #"*" => symbol (str c)
               | #"/" => symbol (str c)
               | #"&" => symbol (str c)
               | #"|" => symbol (str c)
               | other =>
                  (output (stdErr, "invalid symbol: " ^ str(c) ^ "\n"); 
                     OS.Process.exit OS.Process.failure)
         )
         | NONE => (output (stdErr, "no input\n"); TK_NONE)
   end
;

fun recognizeFirstToken fstr =
   if endOfStream fstr
      then TK_EOF
      else case lookahead fstr of
            SOME c => if Char.isAlpha c
               then recognizeIdentifier fstr
               else if Char.isDigit c
                  then recognizeNumber fstr
                  else recognizeSymbol fstr
            | NONE => (output (stdErr, "no input\n"); TK_NONE)
;

fun nextToken fstr =
   if endOfStream fstr
   then TK_EOF
   else recognizeFirstToken (clearWhitespace fstr)
;

