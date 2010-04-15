(* Kaspar Rohrer, Wed Apr 14 13:39:33 CEST 2010 *)

class type context =
object
  method should_expand : Value.t -> bool
  method nesting : int
end

val default_context : context

val make_context : ?nesting:int -> unit -> context

val dump_with_formatter : ?context:context -> Format.formatter -> 'a -> unit
