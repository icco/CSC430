
exception UndefinedIdentifier;

val initial_size = 127;

fun contains map st = isSome (HashTable.find map st);
val lookup = HashTable.lookup;
fun insert map st t = (HashTable.insert map (st, t); map);
fun new_map () = HashTable.mkTable (HashString.hashString, (op =))
   (initial_size, UndefinedIdentifier)
;

(**
 * Take a list and put it into the map/hashtable.
 *
 * if ins is false, only update values that already exist, else insert new ones
 *)
fun update_table map [] (ins:bool) = map
  | update_table map ((id, x)::li) (ins:bool) =
   if contains map id then
      update_table (insert map id x) li ins
   else (
      if ins then
        update_table (insert map id x) li ins
      else
        update_table map li ins
   )
;

fun merge_state s1 s2 =
   update_table (HashTable.copy s2) (HashTable.listItemsi s1) true
;

fun print_map map = 
   HashTable.foldi (fn (x, y, map) => (print (x ^ " : " ^ y ^ "\n"); map)) map
;
