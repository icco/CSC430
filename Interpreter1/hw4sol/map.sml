exception UndefinedIdentifier;

val initial_size = 127;

fun contains map st = isSome (HashTable.find map st);
val lookup = HashTable.lookup;
fun insert map st t = (HashTable.insert map (st, t); map);
fun new_map () = HashTable.mkTable (HashString.hashString, (op =))
   (initial_size, UndefinedIdentifier)
;
