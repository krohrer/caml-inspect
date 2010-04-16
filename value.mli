(* Kaspar Rohrer, Tue Apr 13 15:43:21 CEST 2010 *)

type t = Obj.t

val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int

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

type custom =
  | Custom_nativeint of nativeint
  | Custom_int32 of int32
  | Custom_int64 of int64
  | Custom_bigarray
  | Custom_channel
  | Custom_unknown
  | Not_custom

module TagSet : sig
  include Set.S with type elt = tag

  val all : t
  val of_list : tag list -> t
end

val bits : t -> nativeint
val tag : t -> tag
val heap_words : t -> int
val is_in_heap : t -> bool

val custom_identifier : t -> string
val custom_value : t -> custom
val custom_ops_info : t -> string

val custom_is_int : t -> bool

val custom_has_finalize : t -> bool
val custom_has_compare : t -> bool
val custom_has_hash : t -> bool
val custom_has_serialize : t -> bool
val custom_has_deserialize : t -> bool

val mnemonic : t -> string
val mnemonic_unknown : string

val abbrev : t -> string
val description : t -> string
