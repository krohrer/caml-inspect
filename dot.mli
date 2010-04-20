(* Kaspar Rohrer, Wed Apr 14 02:20:31 CEST 2010 *)

(** Dumping arbitrary values in the Graphviz DOT language

    Context objects are used to configure the dumping process.  A
    sensible [default_context] is already provided. 

    If you are on OS X and have Graphviz installed, you can try the
    [Inspect.Dot.dump_osx] function to dump directly to a
    temporary PDF file, which will then be opened in Preview. *)

type context
  (** The context is used to configure the dumping process *)

type follow = src:Obj.t -> field:int -> dst:Obj.t -> bool
  (** Edge filter predicate *)

val default_context : context
  (** Context with sensible default values, used as the default
      argument for the [dump] function family. *)

val make_context : ?max_fields:int -> ?follow:follow -> unit -> context
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
