(*
* Test file for assignment 1.
*)

use "functions.sml";

" -- Number_of: Empty list Test";
number_of 1 [];

" -- Number_of: One element in list Test";
number_of 4 [1, 2, 3, 4];

" -- Number_of: Multiple elements in list Test (3)";
number_of 4 [1, 2, 3, 4, 4, 9, 12, 30, 4, 60];

"Pair Swap Test";
pair_swap [(1,2), (3,4), (5,6)];

(*
" -- Weave: empty list"
weave [] [];

" -- Weave: equal length lists"
weave [1,2,3] [4,5,6];
*)
