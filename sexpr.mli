(* Kaspar Rohrer, Wed Apr 14 13:39:33 CEST 2010 *)

(** Dumping arbitrary values as S-expression

    Context objects are used to configure the dumping process.  A
    sensible [default_context] is already provided. *)

type context
  (** The context is used to configure the dumping process *)

val default_context : context
  (** Context with sensible default values, used as the default
      argument for the [dump] function family. *)
  
val make_context : ?nesting:int -> unit -> context
  (** Create a custom context. [nesting] is how deep the printer
      recurses before printing *)
  
val dump : ?context:context -> 'a -> unit
  (** Dump to [stdout] *)

val dump_to_file : ?context:context -> string -> 'a -> unit
  (** Dump directly to a file. {b The file will be overwritten if it
      already exists.} *)

val dump_with_formatter : ?context:context -> Format.formatter -> 'a -> unit
  (** Dump using the [Format] module for pretty printing. *)

val test_data : unit -> Obj.t
