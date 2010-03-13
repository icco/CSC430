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

val arithmetic_operators = [OP_PLUS, OP_MINUS, OP_TIMES, OP_DIVIDE];
val relational_operators = [OP_EQ, OP_LT, OP_GT, OP_NE, OP_LE, OP_GE];
val boolean_operators = [OP_AND, OP_OR];

datatype typeA =
   T_BOOL
   | T_INT
   | T_UNIT
   | T_FUNC of typeA list * typeA
   | T_F_V of 
      (typeA * declaration list * declaration list * statement *
         (string, typeA) HashTable.hash_table list)
   | T_ERROR
and declaration =
   DECL of typeA * string
and expression =
   EXP_ID of string
   | EXP_NUM of int
   | EXP_TRUE
   | EXP_FALSE
   | EXP_UNIT
   | EXP_INVOC of string * expression list
   | EXP_BINARY of operator * expression * expression
   | EXP_UNARY of operator * expression
   | EXP_ANON of typeA * declaration list * declaration list * statement
and statement =
   ST_COMPOUND of statement list
   | ST_ASSIGN of string * expression
   | ST_WRITE of expression
   | ST_WRITELINE of expression
   | ST_IF of expression * statement * statement
   | ST_WHILE of expression * statement
   | ST_RETURN of expression
;

datatype function =
   FUNCTION of typeA * string * declaration list * declaration list * statement;

datatype program =
   PROGRAM of declaration list * function list * statement;

datatype value =
     Int_Value of int
   | Bool_Value of bool
   | Unit_Value
   | Func_Value of
      (typeA * declaration list * declaration list * statement *
         (string, value) HashTable.hash_table list)
   | Invalid_Value
;

