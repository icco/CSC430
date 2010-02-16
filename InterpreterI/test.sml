use "interpreter.sml";

val (v, s) = interpret "tests/1_evaluation/eval01.df";

fun print_states [] = ()
  | print_states ((x, y)::xs) = 
  (print (x ^ " = " ^ y "\n"); print_states xs)
;
(*
print_states s;
*)
