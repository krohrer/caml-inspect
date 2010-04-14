(* Kaspar Rohrer, Tue Apr 13 15:36:09 CEST 2010 *)

type t = Obj.t

let compare = compare
let equal = (==)
let hash = Hashtbl.hash

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

let repr = Obj.repr

external bits : t -> nativeint = "inspect_bits"

external custom_identifier : t -> string = "inspect_custom_id"

external custom_has_finalize : t -> bool = "inspect_custom_has_finalize"
external custom_has_compare : t -> bool = "inspect_custom_has_compare"
external custom_has_hash : t -> bool = "inspect_custom_has_hash"
external custom_has_serialize : t -> bool = "inspect_custom_has_serialize"
external custom_has_deserialize : t -> bool = "inspect_custom_has_deserialize"

let custom_info r =
  Printf.sprintf "%c%c%c%c%c"
    (if custom_has_finalize r    then 'F' else '-')
    (if custom_has_compare r     then 'C' else '-')
    (if custom_has_hash r        then 'H' else '-')
    (if custom_has_serialize r   then 'S' else '-')
    (if custom_has_deserialize r then 'D' else '-')

let nativeint_id = "_n"
let int32_id = "_i"
let int64_id = "_j"
let bigarray_id = "_bigarray"
let channel_id = "_chan"

(* Make sure the known custom identifiers are in sync. *)
let _ =
  let rnat = repr 0n and ri32 = repr 0l and ri64 = repr 0L in
    assert (Obj.tag rnat = Obj.custom_tag);
    assert (Obj.tag ri32 = Obj.custom_tag);
    assert (Obj.tag ri64 = Obj.custom_tag);
    assert (nativeint_id = custom_identifier rnat);
    assert (int32_id = custom_identifier ri32);
    assert (int64_id = custom_identifier ri64);
    (* assert (bigarray_id = custom_identifier ...); *)
    assert (channel_id = custom_identifier (repr stdout));
    ()

let custom_value r =
  if Obj.tag r = Obj.custom_tag then (
    let id = custom_identifier r in
      if id = nativeint_id then
	Custom_nativeint (Obj.magic r : nativeint)
      else if id = int32_id then
	Custom_int32 (Obj.magic r : int32)
      else if id = int64_id then
	Custom_int64 (Obj.magic r : int64)
      else if id = channel_id then
	Custom_channel
      else if id = bigarray_id then
	Custom_bigarray
      else
	Custom_unknown
  )
  else
    Not_custom

let custom_is_int r =
  match custom_value r with
    | Custom_nativeint _ -> false
    | Custom_int32 _ -> true
    | Custom_int64 _ ->	true
    | _ -> false

(* Matching an integer value should be faster than a series of if
   statements.
   That's why all these assertions are here, to make sure
   that the integer literals used in the match statement actually
   correspond to the tags defined by the Obj module. *)
let _ =
  assert (Obj.lazy_tag = 246);
  assert (Obj.closure_tag = 247);
  assert (Obj.object_tag = 248);
  assert (Obj.infix_tag = 249);
  assert (Obj.forward_tag = 250);
  assert (Obj.no_scan_tag = 251);
  assert (Obj.abstract_tag = 251);
  assert (Obj.string_tag = 252);
  assert (Obj.double_tag = 253);
  assert (Obj.double_array_tag = 254);
  assert (Obj.custom_tag = 255);
  assert (Obj.int_tag = 1000);
  assert (Obj.out_of_heap_tag = 1001);
  assert (Obj.unaligned_tag = 1002);
  ()

(* Slower and safer.
let value_tag r =
  match tag r with
    | x when x = lazy_tag -> Lazy
    | x when x = closure_tag -> Closure
    | x when x = object_tag -> Object
    | x when x = infix_tag -> Infix
    | x when x = forward_tag -> Forward
    | x when x < no_scan_tag -> Block
    | x when x = abstract_tag -> Abstract
    | x when x = string_tag -> String
    | x when x = double_tag -> Double
    | x when x = double_array_tag -> Double_array
    | x when x = custom_tag -> Custom
    | x when x = int_tag -> Int
    | x when x = out_of_heap_tag -> Out_of_heap
    | x when x = unaligned_tag -> Unaligned
    | x -> failwith (sprintf "OCaml value with unknown tag = %d" x)
*)

(* Faster but more dangerous *)
let tag r =
  match Obj.tag r with
    | x when x < 246 -> Block
    | 246 -> Lazy
    | 247 -> Closure
    | 248 -> Object
    | 249 -> Infix
    | 250 -> Forward
    | 251 -> Abstract
    | 252 -> String
    | 253 -> Double
    | 254 -> Double_array
    | 255 -> Custom
    | 1000 -> Int
    | 1001 -> Out_of_heap
    | 1002 -> Unaligned
    | x -> failwith (Printf.sprintf "OCaml value with unknown tag = %d" x)

(* Slower? and safer
let is_in_heap r =
  let x = Obj.tag r in
    not (x = Obj.int_tag || x = Obj.out_of_heap_tag || x = Obj.unaligned_tag)
*)

(* Faster but more dangerous *)
let is_in_heap r =
  let x = Obj.tag r in
    x < 1000 || 1002 < x

let heap_words r =
  if is_in_heap r then Obj.size r else 0

let mnemonic r = 
  match tag r with
    | Lazy -> "LAZY"
    | Closure -> "CLOS"
    | Object -> "OBJ"
    | Infix -> "INFX"
    | Forward -> "FWD"
    | Block -> Printf.sprintf "BL%d" (Obj.tag r)
    | Abstract -> "ABST"
    | String -> "STR"
    | Double -> "DBL"
    | Double_array -> "DBLA"
    | Custom -> "CUST"
    | Int -> "INT"
    | Out_of_heap -> "OADR"
    | Unaligned -> "UADR"

let mnemonic_unknown =
  "????"

let description r =
  match tag r with
    | Lazy         -> "Lazy: #" ^ string_of_int (Obj.size r)
    | Closure      -> "Closure: #" ^ string_of_int (Obj.size r)
    | Object       -> "Object: #" ^ string_of_int (Obj.size r)
    | Infix        -> "Infix: #" ^ string_of_int (Obj.size r)
    | Forward      -> "Forward: #" ^ string_of_int (Obj.size r)
    | Block        -> Printf.sprintf "Block[%d]: #%d" (Obj.tag r) (Obj.size r)
    | Abstract     -> "Abstract: #" ^ string_of_int (Obj.size r)
    | String       -> Printf.sprintf "String: %d chars)" (String.length (Obj.magic r : string))
    | Double       -> Printf.sprintf "Double: %g" (Obj.magic r : float)
    | Double_array -> Printf.sprintf "Double_array: %d floats" (Array.length (Obj.magic r : float array))
    | Custom       -> (
	match custom_value r with
	  | Custom_nativeint n -> Printf.sprintf "Nativeint: %nd" n
	  | Custom_int32 i     -> Printf.sprintf "Int32: %ld" i
	  | Custom_int64 i     -> Printf.sprintf "Int64: %Ld" i
	  | Custom_bigarray    -> "Bigarray"
	  | Custom_channel     -> "Channel"
	  | Custom_unknown     -> Printf.sprintf "Custom: %S" (custom_identifier r)
	  | Not_custom         -> failwith "Value.description: should be a custom value"
      )
    | Int          -> Printf.sprintf "Int: %d" (Obj.magic r : int)
    | Out_of_heap  -> Printf.sprintf "Out_of_heap (0x%nX)" (bits r)
    | Unaligned    -> Printf.sprintf "Unaligned (0x%nX)" (bits r)
