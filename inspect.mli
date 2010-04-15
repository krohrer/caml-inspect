(* Kaspar Rohrer, Wed Apr 14 14:52:25 CEST 2010 *)

(** Inspection of internal value representations and the object graph.

    This small library can be used to inspect arbitrary OCaml values,
    either by dumping the object graph as pretty printed S-expressions
    with references and sharing, or by generating output in the
    DOT-language to be further processed by Graphviz. This
    functionality is provided by the [Sexpr] and [Dot] sub-modules.

    Context objects are used to configure the dumping process, and
    sensible default contexts are already provided in the respective
    sub modules.

    If you are on OS X and have Graphviz installed, you can try the
    [Inspect.Dot.dump_command] function to dump directly to a
    temporary PDF file, which will then be opened in Preview.
*)

val count_heap_words_and_objects : 'a -> int * int
  (** Count the number of words the object graph occupies in the OCaml
      value heap, and the number of distinct nodes. The size of a word
      is given by [Sys.word_size].
      
      {i NOTE:} The number of heap words does not include the block header.
      Simply adding the number of objects to the total should do the trick, though. *)

(** Dumping arbitrary values as S-expression *)
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
    (** Dump to [stdout] *)

  val dump_to_file : ?context:context -> string -> 'a -> unit
    (** Dump directly to a file. {b The file will be overwritten if it
	already exists.} *)

  val dump_with_formatter : ?context:context -> Format.formatter -> 'a -> unit
    (** Dump using the [Format] module for pretty printing. *)

  val test_data : unit -> Obj.t
end

(** Dumping arbitrary values in the Graphviz DOT language *)
module Dot :
sig
  type context
    (** The context is used to configure the dumping process *)

  val default_context : context
    (** Context with sensible default values, used as the default
	argument for the [dump] function family. *)

  val make_context : ?max_fields:int -> unit -> context
    (** Create a custom context. [max_fields] controls how many fields
	should be expanded for block nodes. *)

  val dump : ?context:context -> 'a -> unit
    (** Dump to [stdout] *)

  val dump_to_file : ?context:context -> string -> 'a -> unit
    (** Dump directly to a file. {b The file will be overwritten if it
	already exists.} *)

  val dump_with_formatter : ?context:context -> Format.formatter -> 'a -> unit
    (** Dump using the [Format] module for pretty printing. *)

  val dump_osx : ?context:context -> ?cmd:string -> ?format:string -> 'a -> unit
    (** [dump_osx ?context ?cmd ?format o] dumps the value [o] to a
	temporary file, runs the Graphviz program given by [cmd] on it
	to generate output as specified by [format], and then opens
	the generated output with the [open] command. The default output format is "pdf".

	E.g. [Inspect.Dot.dump_osx ~cmd:"neato" (Inspect.test_data ())]

	This function will block while the graph is being generated. *)

  val test_data : unit -> Obj.t
    (** Generate test data to inspect *)
end
