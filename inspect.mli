(* Kaspar Rohrer, Wed Apr 14 14:52:25 CEST 2010 *)

val count_heap_words_and_objects : 'a -> int * int

val test_data : unit -> Obj.t

val dump : 'a -> unit
val dot : 'a -> unit
