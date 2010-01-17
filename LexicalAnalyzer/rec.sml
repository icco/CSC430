use "tok.sml";

fun recognizeToken nil = ()
  | recognizeToken instr =
   if TextIO.endOfStream instr then 
     print "End Of File"
   else
     case (read_token instr) of
          (TOKEN x) => print x
        | NONE => ()
;
