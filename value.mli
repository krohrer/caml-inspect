(** Arbitrary values (based on the [Obj] module) 
    
    This module builds upon the {!Obj} module. It provides the {!tag}
    and {!custom} types to provide better dispatch on the internal
    type of any OCaml value. It also provides functions to inspect the
    representation of values, and to generate human-readable
    descriptions and mnemonics for use by the {!Dot} and {!Sexpr}
    module.
*)

(** {6 Types} *)

type t = Obj.t
(** [Value.t] is the same as [Obj.t] *)

(** This allows better dispatch than comparing the tags. *)
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

(** Information about custom types *)
type custom =
  | Custom_nativeint of nativeint
  | Custom_int32 of int32
  | Custom_int64 of int64
  | Custom_bigarray
  | Custom_channel
  | Custom_unknown
  | Not_custom

(** A set of tags *)
module TagSet : sig
  include Set.S with type elt = tag

  val all : t
    (** All tags in a set *)

  val of_list : tag list -> t
    (** Conversion from list to set *)
end

(** {6 Hashtbl.HashedType} *)

(** Equality predicate (physical equality only!) *)
val equal : t -> t -> bool

(** Hashing function (used*)
val hash : t -> int

(** {6 Value representation} *)

val bits : t -> nativeint
(** Return the raw bits of a value *)

val bits_to_string : ?base:[`Dec|`Hex|`Bin] -> t -> string
(** Return a string describing the low-level bit pattern of a
    value. *)

val tag : t -> tag
(** Tag type of a value *)

val heap_words : t -> int
  (** Number of words that the value occupies on the heap (without
      the block header). *)

val is_in_heap : t -> bool
  (** [Int], [Out_of_heap] and [Unaligned] are out. *)

(** {6 Custom Blocks} *)

val custom_identifier : t -> string
  (** Returns the identifier of the custom block  *)

val custom_value : t -> custom
  (**  *)

val custom_ops_info : t -> string
  (** Info about the available operations (finalize / compare / hash
      / serialize / deserialize), which the custom block provides.

      E.g. {e FCHSD}, {e FC---}, {e -----}
  *)

val custom_is_int : t -> bool
  (** Is the custom value a [int32], [int64] or [nativeint]?*)
  
val custom_has_finalize : t -> bool
  (** Supports finalization? *)

val custom_has_compare : t -> bool
  (** Supports comparison? *)

val custom_has_hash : t -> bool
  (** Supports hashing? *)

val custom_has_serialize : t -> bool
  (** Supports serialization? *)
  
val custom_has_deserialize : t -> bool
  (** Supports deserialization? *)

(** {6 Value Descriptions} *)

val mnemonic : t -> string
  (** Mnemonic or identifier for a value *)

val mnemonic_unknown : string
  (** Mnemonic for an unknown/abstract value, *)

val abbrev : t -> string
  (** Abbreviated, readable description of a value*)

val description : t -> string
  (** Readable description of a value*)
