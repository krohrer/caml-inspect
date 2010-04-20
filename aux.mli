(* Kaspar Rohrer, Tue Apr 20 18:57:19 CEST 2010 *)

(** Auxiliary functions for general use. *)

val with_file_out_channel : string -> (out_channel -> unit) -> unit
  (** [with_file_out_channel path writer] tries to open a new
      [out_channel] for writing to the file at [path]. It then applies
      [writer] to the resulting [out_channel]. Finally, it closes the
      out_channel, even if an exception was raised.

      {b Note:} The file will be truncated if it already exists. *)

val with_buffer : int -> (Buffer.t -> unit) -> string
  (** [with_buffer size writer] creates a new [Buffer.t] with space
      for at least [size] characters. It then applies [writer] to the
      resulting buffer, and returns the contents of it.

      The idea is to be able to reuse the buffer by partial
      application. *)

val (|>>) : 'a -> ('a -> 'b) -> 'b
  (** Pipeline operator (apply function to previous result) *)
