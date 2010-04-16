(* Kaspar Rohrer, Wed Apr 14 02:20:31 CEST 2010 *)

type dot_attrs = (string * string) list

class type context =
object
  method graph_attrs : dot_attrs
  method all_nodes_attrs : dot_attrs
  method all_edges_attrs : dot_attrs
  method node_attrs : ?root:bool -> Obj.t -> dot_attrs
  method edge_attrs : src:Obj.t -> field:int -> dst:Obj.t -> dot_attrs

  method should_inline : Obj.t -> bool
  method should_follow_edge : src:Obj.t -> field:int -> dst:Obj.t -> bool
  method max_fields_for_node : Obj.t -> int
end

val default_context : context

val make_context : ?max_fields:int -> unit -> context

val dump_with_formatter : ?context:context -> Format.formatter -> 'a -> unit
