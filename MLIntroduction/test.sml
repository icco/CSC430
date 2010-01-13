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
*)

(*
"Pair_Swap: 3 Tests";
pair_swap [(1,2)];
pair_swap [(1,2), (3,4), (5,6)];
pair_swap [];
*)

(*
" -- Weave: empty list";
1::(weave [] []); (* 1 cons'd in to prevent type warning *)

" -- Weave: equal length lists";
weave [1,2,3] [4,5,6];

" -- Weave: different length lists";
weave [1,2] [3];
" -- Weave: Imbalanced exception errors";
(weave [1,2,3] [3]) handle ImbalancedWeaving => [];
(weave [1] [2,3]) handle ImbalancedWeaving => [];
*)

(*
" -- file_subst: No replacements";
file_subst "file.txt" [] [];
" -- file_subst: replacements";
file_subst "file.txt" ["t"] ["x"];
" -- file_subst: no file";
file_subst "" [] [];
*)

" -- number_of_things 0,1,5";
number_of_things Nothing;
number_of_things (OneThing (7, Nothing));
number_of_things (OneThing (7, ManyThings ([1, 2], TwoThings (1, 2, Nothing))));

" -- number_of_OneThing 0, 1, 1, 2";
number_of_OneThing Nothing;
number_of_OneThing (OneThing (7, Nothing));
number_of_OneThing (OneThing (7, ManyThings ([1, 2], TwoThings (1, 2, Nothing))));
number_of_OneThing (OneThing (7, ManyThings ([1, 2], TwoThings (1, 2, OneThing (99, Nothing)))));
