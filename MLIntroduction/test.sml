(*
* Test file for assignment 1.
*)

use "functions.sml";

(*
" -- Number_of: Empty list Test";
number_of 1 [];

" -- Number_of: One element in list Test";
number_of 4 [1, 2, 3, 4];

" -- Number_of: Multiple elements in list Test (3)";
number_of 4 [1, 2, 3, 4, 4, 9, 12, 30, 4, 60];

"Pair_Swap: 3 Tests";
pair_swap [(1,2)];
pair_swap [(1,2), (3,4), (5,6)];
pair_swap [];

" -- Weave: empty list";
1::(weave [] []); (* 1 cons'd in to prevent type warning *)

" -- Weave: equal length lists";
weave [1,2,3] [4,5,6];

" -- Weave: different length lists";
weave [1,2] [3];
" -- Weave: Imbalanced exception errors";
(weave [1,2,3] [3]) handle ImbalancedWeaving = [];
(weave [1] [2,3]) handle ImbalancedWeaving = [];
" -- file_subst: No replacements";
file_subst "file.txt" [] [];

" -- file_subst: replacements";
file_subst "file.txt" ["t"] ["x"];

" -- file_subst: no file";
file_subst "" [] [];
*)

open TextIO;
val fstr = openIn("file.txt");
fun readList infile =
   if endOfStream infile then []
   else (inputN(infile,1))::(readList infile);
