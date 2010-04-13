(* Kaspar Rohrer, Thu Apr  8 02:00:21 CEST 2010 *)

type tag =
  | Lazy
  | Closure
  | Object
  | Infix
  | Forward
  | Block
  | Abstract
  | String
  | Double
  | Double_array
  | Custom
  | Int
  | Out_of_heap
  | Unaligned

module Tags : sig
  include Set.S with type elt = tag

  val all : t
  val of_list : tag list -> t
end

type dot_attrs = (string * string) list

class type dot_context =
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

class type dump_context =
object
  method should_expand : tag -> bool
  method max_depth : int
end

val default_dot_context : dot_context
val default_dump_context : dump_context

(* [max_size] can be negative as well, in which case the record does
   not even show if there are additional fields. *)
val dot : ?context:dot_context -> 'a -> unit

val dot_osx : ?context:dot_context -> 'a -> bool

val dot_to_file : ?context:dot_context -> string -> 'a -> unit



val dump : ?context:dump_context -> 'a -> unit

val dump_to_string : ?context:dump_context -> 'a -> string

val dump_to_buffer : ?context:dump_context -> Buffer.t -> 'a -> unit

val dump_to_channel : ?context:dump_context -> out_channel -> 'a -> unit



val heap_size : ?tags:Tags.t -> ?follow:Tags.t -> 'a -> int



val test_data : unit -> Obj.t
