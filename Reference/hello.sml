(* Some code examples I whipped up *)

print "Hello world!\n";

fun two_var a b = if a > b then a else b;

fun factorial n = if n = 0 then 1 else n * factorial (n-1);

open TextIO;
val fstr = openIn("file.txt");
fun readList infile =
   if endOfStream infile then []
   else (inputN(infile,1))::(readList infile);

fun map g l =
  foldr (fn (head, res) => (g head)::res) [] l
;

fun sum [] = 0
  | sum (x::xs) =
  x + sum xs
;

fun isOneThing (OneThing x) = 1
  | isOneThing x = 0
;

