(* Kaspar Rohrer, Wed Apr 14 13:39:33 CEST 2010 *)

class type context =
object
  method is_not_too_deep : depth:int -> Value.t -> bool
  method should_expand : Value.t -> bool
end

val default_context : context

val make_context : ?nesting:int -> unit -> context

val dump_with_formatter : ?context:context -> Format.formatter -> 'a -> unit
