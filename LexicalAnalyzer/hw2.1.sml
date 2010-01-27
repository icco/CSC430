open TextIO;

val keywords = ["int", "bool", "fn", "write", "writeline",
   "if", "else", "while", "true", "false", "return", "var", "unit"];

fun member a [] = false
  | member a (x::xs) = if a = x then true else member a xs
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
         then (output (stdErr, "keyword: " ^ id ^ "\n"); ())
         else (output (stdErr, "identifier: " ^ id ^ "\n"); ())
   end
;

fun recognizeNumber fstr =
   let
      val id = buildToken fstr Char.isDigit ""
   in 
      (output (stdErr, "number: " ^ id ^ "\n"); ())
   end
;

fun symbol str =
   (output (stdErr, "symbol: " ^ str ^ "\n"); ())
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
         | NONE => (output (stdErr, "no input\n"); ())
   end
;

fun recognizeFirstToken fstr =
   if endOfStream fstr
      then (output (stdErr, "end-of-file\n"); ())
      else case lookahead fstr of
            SOME c => if Char.isAlpha c
               then recognizeIdentifier fstr
               else if Char.isDigit c
                  then recognizeNumber fstr
                  else recognizeSymbol fstr
            | NONE => (output (stdErr, "no input\n"); ())
;

fun recognizeToken fstr =
   if endOfStream fstr
   then (output (stdErr, "end-of-file\n"); ())
   else recognizeFirstToken (clearWhitespace fstr)
;

