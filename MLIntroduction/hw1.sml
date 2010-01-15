(* Keen's Solution... *)


fun number_of v [] =
      0
  | number_of v (x::xs) =
      (if v = x then 1 else 0) + (number_of v xs)
;

fun pair_swap [] =
      []
  | pair_swap ((x,y)::pairs) =
      (y,x)::(pair_swap pairs)
;

exception ImbalancedWeaving;

fun weave_first [] [] =
      []
  | weave_first (x::xs) ys =
      x :: (weave_second xs ys)
  | weave_first [] ys =
      raise ImbalancedWeaving
and weave_second [] [] =
      []
  | weave_second xs (y::ys) =
      y :: (weave_first xs ys)
  | weave_second xs [] =
      raise ImbalancedWeaving
;

fun weave xs ys =
   weave_first xs ys
;

fun char_subst [] (c : char) =
      c
  | char_subst ((x, y)::pairs) c =
      if x = c
      then y
      else char_subst pairs c
;

fun echo_subst fstr pairs =
   if TextIO.endOfStream fstr
   then ()
   else
      case (TextIO.input1 fstr) of
         (SOME x) =>
            (TextIO.print (str (char_subst pairs x))
            ; echo_subst fstr pairs)
         | NONE => ()
;

fun file_subst file pairs =
   let
      val fstr = TextIO.openIn file
   in
      echo_subst fstr pairs
   end
;

datatype 'a ThingCollection =
     OneThing of ('a * 'a ThingCollection)
   | TwoThings of ('a * 'a * 'a ThingCollection)
   | ManyThings of ('a list * 'a ThingCollection)
   | Nothing
;

fun number_of_things (OneThing (_, rest)) =
      1 + number_of_things rest
  | number_of_things (TwoThings (_, _, rest)) =
      2 + number_of_things rest
  | number_of_things (ManyThings (things, rest)) =
      (length things) + number_of_things rest
  | number_of_things Nothing =
      0
;

fun number_of_OneThing (OneThing (_, rest)) =
      1 + number_of_OneThing rest
  | number_of_OneThing (TwoThings (_, _, rest)) =
      number_of_OneThing rest
  | number_of_OneThing (ManyThings (_, rest)) =
      number_of_OneThing rest
  | number_of_OneThing Nothing =
      0
;

fun number_of_XThing p things =
   (if p things then 1 else 0)
   +
   (case things of (OneThing (_, rest)) =>
      number_of_XThing p rest
     | (TwoThings (_, _, rest)) =>
      number_of_XThing p rest
     | (ManyThings (_, rest)) =>
      number_of_XThing p rest
     | Nothing =>
      0
   )
;

fun number_of_TwoThings things =
   number_of_XThing (fn (TwoThings _) => true | _ => false) things;

fun map_thing_collection f (OneThing (x, rest)) =
      OneThing (f x, map_thing_collection f rest)
  | map_thing_collection f (TwoThings (x, y, rest)) =
      TwoThings (f x, f y, map_thing_collection f rest)
  | map_thing_collection f (ManyThings (l, rest)) =
      ManyThings (map f l, map_thing_collection f rest)
  | map_thing_collection f Nothing =
      Nothing
;

fun flatten_collection_h (OneThing (x, rest)) =
      x :: (flatten_collection_h rest)
  | flatten_collection_h (TwoThings (x, y, rest)) =
      x :: y :: (flatten_collection_h rest)
  | flatten_collection_h (ManyThings (things, rest)) =
      things @ (flatten_collection_h rest)
  | flatten_collection_h  Nothing =
      []
;

fun flatten_collection things =
   ManyThings (flatten_collection_h things, Nothing)
;
