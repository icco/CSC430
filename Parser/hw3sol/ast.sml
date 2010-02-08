datatype operator =
   OP_PLUS
   | OP_MINUS
   | OP_TIMES
   | OP_DIVIDE
   | OP_EQ
   | OP_LT
   | OP_GT
   | OP_NE
   | OP_LE
   | OP_GE
   | OP_NOT
   | OP_AND
   | OP_OR
;

datatype expression =
   EXP_ID of string
   | EXP_NUM of int
   | EXP_TRUE
   | EXP_FALSE
   | EXP_UNIT
   | EXP_INVOC of string * expression list
   | EXP_BINARY of operator * expression * expression
   | EXP_UNARY of operator * expression
;

datatype statement =
   ST_COMPOUND of statement list
   | ST_ASSIGN of string * expression
   | ST_WRITE of expression
   | ST_WRITELINE of expression
   | ST_IF of expression * statement * statement
   | ST_WHILE of expression * statement
   | ST_RETURN of expression
;

datatype declaration =
   DECL of string;

datatype function =
   FUNCTION of string * declaration list * declaration list * statement;

datatype program =
   PROGRAM of declaration list * function list * statement;

