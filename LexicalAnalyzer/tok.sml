datatype 'a TokenType =
    TOKEN of (`a)
  | NONE
;

fun read_token nil = NONE
  | read_token instr =
   TOKEN (TextIO.input1 instr)
;
