(* Kaspar Rohrer, Wed Apr 14 14:52:25 CEST 2010 *)

(** Inspection of internal value representations and object graph.

    This small library can be used to inspect arbitrary OCaml values,
    either by dumping the object graph as pretty printed S-expressions
    with references and sharing, or by generating output in the
    DOT-language to be further processed by Graphviz.

    If you are on OS X and have Graphviz installed, you can try the
    [Inspect.Dot.dump_command] function to dump directly to a
    temporary PDF file, which will then be opened in Preview.
*)

(** {6 Dumping S-expression} *)
module Sexpr :
sig
  type context
    (** The context is used to configure the dumping process *)

  val default_context : context
    (** Context with sensible default values, used as the default
	argument for the [dump] function family. *)
    
  val make_context : ?nesting:int -> unit -> context
    (** Create a custom context. [nesting] is how deep the printer
	recurses before it prints references *)
    
  val dump : ?context:context -> 'a -> unit
    (** Dump any value to [stdout] *)

  val dump_to_string : ?context:context -> 'a -> string
    (** Dump any value as a string *)

  val dump_to_buffer : ?context:context -> Buffer.t -> 'a -> unit
    (** Dump any value into a string buffer *)

  val dump_to_channel : ?context:context -> out_channel -> 'a -> unit
    (** Dump any value to an [out_channel] *)
end

(** {6 Dumping Graphviz format} *)
module Dot :
sig
  type context
    (** The context is used to configure the dumping process *)

  val default_context : context
    (**  *)

  val make_context : ?max_size:int -> unit -> context
    (** *)

  val dump : ?context:context -> 'a -> unit
    (**  *)

  val dump_to_file : ?context:context -> string -> 'a -> unit
    (**  *)

  val dump_command : ?context:context -> ?cmd:string -> 'a -> unit
    (** It is currently only supported on Mac OS X *)
end

(* {6 Misc} *)

val count_heap_words_and_objects : 'a -> int * int
  (** Count the number of words the object graph occupies in the OCaml
      value heap, and the number of distinct nodes. The size of a word
      is given by [Sys.word_size].
      
      {i NOTE:} The number of heap words does not include the block header.
      Simply adding the number of objects to the total should do the trick, though. *)

val test_data : unit -> Obj.t
  (** Generate test data to inspect *)

