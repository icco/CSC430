(*
* Test file for assignment 1.
*)

use "functions.sml";

"Empty list Test";
number_of 1 [];

"One element in list Test";
number_of 4 [1, 2, 3, 4];

"Multiple elements in list Test (3)";
number_of 4 [1, 2, 3, 4, 4, 9, 12, 30, 4, 60];

"Pair Swap Test";
pair_swap [(1,2), (3,4), (5,6)];
