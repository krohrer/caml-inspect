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

val dump_list : ?context:context -> (string * 'a) list -> unit
  (** Same as {!dump} but for a list of labeled values.*)

val dump_to_file : ?context:context -> string -> 'a -> unit
  (** Dump directly to a file. {b The file will be overwritten if it
      already exists.} *)

val dump_list_to_file : ?context:context -> string -> (string * 'a) list -> unit
  (** Same as {!dump_to_file} but for a list of labeled values.*)

val dump_with_formatter : ?context:context -> Format.formatter -> 'a -> unit
  (** Dump using the [Format] module for pretty printing. *)

val dump_list_with_formatter : ?context:context -> Format.formatter -> (string * 'a) list -> unit
  (** Same as {!dump_with_formatter} but for a list of labeled values.*)

val dump_and_open : ?context:context -> ?cmd:string -> format:string -> viewer:string -> 'a -> unit
  (** [dump_and_open ?context ?cmd ~format ~viewer o] dumps the value [o] to a
      temporary file, runs the Graphviz program given by [cmd] on it
      to generate output as specified by [format], and then opens
      the generated output with the [viewer] command.

      E.g. [Inspect.Dot.dump_osx ~cmd:"neato" (Inspect.test_data ())]

      This function will block while the graph is being generated. *)

val dump_list_and_open : ?context:context -> ?cmd:string ->
  format:string -> viewer:string -> (string * 'a) list -> unit
  (** Same as {!dump_and_open} but for a list of labeled values.*)

val dump_osx : ?context:context -> ?cmd:string -> 'a -> unit
  (** Call {!val:dump_and_open} with [format] "pdf" and [viewer] "open". *)

val dump_list_osx : ?context:context -> ?cmd:string -> (string * 'a) list -> unit
  (** Same as {!dump_osx} but for a list of labeled values.*)

val test_data : unit -> Obj.t
  (** Generate test data to inspect *)
