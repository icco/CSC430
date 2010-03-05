use "map.sml";
use "ast.sml";

datatype value =
     Int_Value of int
   | Bool_Value of bool
   | Unit_Value
   | Func_Value of (declaration list * declaration list * statement)
   | Clojure_Value of (* anonomous func *) * (* state list *)
   | Invalid_Value
;

