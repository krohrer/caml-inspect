(* Kaspar Rohrer, Wed Apr 14 02:20:31 CEST 2010 *)

type dot_attrs = (string * string) list

class type context =
object
  method graph_attrs : dot_attrs
  method all_nodes_attrs : dot_attrs
  method all_edges_attrs : dot_attrs
  method node_attrs : ?root:bool -> label:string -> Obj.t -> dot_attrs
  method edge_attrs : src:Obj.t -> field:int -> dst:Obj.t -> dot_attrs

  method should_expand_node : Obj.t -> bool
  method should_follow_edge : src:Obj.t -> field:int -> dst:Obj.t -> bool
  method max_size : int
end

val default_context : context

(* [max_size] can be negative as well, in which case the record does
   not even show if there are additional fields. *)
val dump : ?context:context -> 'a -> unit

val dump_to_file : ?context:context -> string -> 'a -> unit

val dump_command : ?context:context -> 'a -> bool
