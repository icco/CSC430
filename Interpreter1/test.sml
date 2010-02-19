(* Test file... *)

use "interpreter.sml";

print "---- Test 1\n";
interpret "tests/2_dynamic_typing/type_error17.df";
interpret "tests/2_dynamic_typing/type_error13.df";
interpret "tests/2_dynamic_typing/type_error03.df";

