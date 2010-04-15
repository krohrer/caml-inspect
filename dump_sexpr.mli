(* Kaspar Rohrer, Wed Apr 14 13:39:33 CEST 2010 *)

class type context =
object
  method should_expand : Value.t -> bool
  method nesting : int
end

val default_context : context

val make_context : ?nesting:int -> unit -> context

val dump : ?context:context -> 'a -> unit

val dump_to_string : ?context:context -> 'a -> string

val dump_to_buffer : ?context:context -> Buffer.t -> 'a -> unit

val dump_to_channel : ?context:context -> out_channel -> 'a -> unit

