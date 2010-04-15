(* Kaspar Rohrer, Wed Apr 14 13:46:21 CEST 2010 *)

open Format

module HT = Hashtbl.Make(Value)

class type context =
object
  method is_not_too_deep : depth:int -> Value.t -> bool
  method should_expand : Value.t -> bool
end

let make_context ?(nesting=20) () =
object
  method is_not_too_deep ~depth r =
    depth < nesting

  method should_expand r =
    match Value.tag r with
      | _ -> true
end

let default_context = make_context ()

(*----------------------------------------------------------------------------*)

let dump_with_formatter ?(context=default_context) fmt o =
  let queue = Queue.create () in
  let indentation_for_string id = 3 (* String.length id + 2 *) in

  let rec value2id = HT.create 31337
  and id_of_value r =
    try
      id_find r
    with Not_found -> (
      let tid = Value.mnemonic r in
      let n = HT.length value2id in
      let id = sprintf "%s/%d" tid n in
	HT.add value2id r id;
	id
    )
  and id_find r =
    HT.find value2id r
  in

  let sexpr_open fmt id =
    fprintf fmt "@[<hv %d>(%s" (indentation_for_string id) id
  and sexpr_close fmt () =
    fprintf fmt ")@]"
  and sexpr_sep fmt () =
    fprintf fmt "@ "
  (* and sexpr_mnemo fmt r = *)
  (*   pp_print_string fmt (Value.mnemonic r) *)
  and sexpr_ref fmt id =
    fprintf fmt "@@%s" id
  in

  let rec sexpr_one body ~depth fmt r =
    if depth = 0 then (
      (* At depth 0 we are never too deep, no mather what the (whiny)
	 context might say. We have to consume the queue somehow. *)
      sexpr_open fmt (id_of_value r);
      body ();
      sexpr_close fmt ()
    )
    else (
      if context#is_not_too_deep ~depth r then (
	(* It still is reasonable to dump this *)
	try sexpr_ref fmt (id_find r) with Not_found ->
	  (* Print reference to already printed sexpr, or print now *)
	  sexpr_open fmt (id_of_value r);
	  body ();
	  sexpr_close fmt ()
      )
      else (
	try sexpr_ref fmt (id_find r) with Not_found ->
	  (* Print reference to already printed sexpr, or queue for later *)
	  Queue.add r queue
      )
    )
	
  and sexpr_string ~depth fmt r =
    let body () =
      assert (Obj.tag r = Obj.string_tag);
      let s : string = Obj.magic r in
	sexpr_sep fmt ();
	fprintf fmt ":BYTES %d" (String.length s);
	if context#should_expand r then (
	  sexpr_sep fmt ();
	  fprintf fmt "%S" s
	)
    in
      sexpr_one body ~depth fmt r

  and sexpr_abstract ~depth fmt r=
    let body () =
      assert (Obj.tag r = Obj.abstract_tag);
      sexpr_sep fmt ();
      fprintf fmt ":SIZE %d" (Value.heap_words r);
    in
      sexpr_one body ~depth fmt r

  and sexpr_custom ~depth fmt r =
    let body () =
      assert (Obj.tag r = Obj.custom_tag);
      sexpr_sep fmt ();
      fprintf fmt ":ID %S" (Value.custom_identifier r);
      if context#should_expand r then (
	sexpr_sep fmt ();
	fprintf fmt ":SIZE %d" (Value.heap_words r);
	sexpr_sep fmt ();
	fprintf fmt ":OPS %s" (Value.custom_ops_info r)
      )
    in
      sexpr_one body ~depth fmt r

  and sexpr_block ~depth fmt r =
    let body () =
      assert (Obj.tag r < Obj.no_scan_tag);
      let n = Obj.size r and depth = depth + 1 in
	if context#should_expand r then (
	  for i = 0 to n - 1 do
	    let f = Obj.field r i in
	      sexpr_sep fmt ();
	      sexpr_value ~depth fmt f
	  done
	)
	else (
	  sexpr_sep fmt ();
	  fprintf fmt ":SIZE %d" n
	)
    in
      sexpr_one body ~depth fmt r

  and sexpr_double_array ~depth fmt r =
    let body () =
      assert (Obj.tag r = Obj.double_array_tag);
      let a : float array = Obj.magic r in
      let n = Array.length a in
	if context#should_expand r then (
	  for i = 0 to n - 1 do
	    sexpr_sep fmt ();
	    fprintf fmt "%g" a.(i)
	  done
	)
	else (
	  sexpr_sep fmt ();
	  fprintf fmt ":SIZE %d" n
	)
    in
      sexpr_one body ~depth fmt r

  and sexpr_float fmt r =
    assert (Obj.tag r = Obj.double_tag);
    fprintf fmt "%f" (Obj.magic r : float)

  and sexpr_int fmt r =
    assert (Obj.tag r = Obj.int_tag);
    fprintf fmt "%d" (Obj.magic r : int)

  and sexpr_nativeint fmt ni =
    fprintf fmt "%ndn" ni

  and sexpr_int32 fmt i32 =
    fprintf fmt "%ldl" i32

  and sexpr_int64 fmt i64 =
    fprintf fmt "%LdL" i64

  and sexpr_addr fmt r =
    fprintf fmt "0x%nX" (Value.bits r)

  and sexpr_value ~depth fmt r =
    (* The great dispatch! I wonder how this would look in Java *)
    let t = Value.tag r in
      match t with
	| Value.Lazy 
	| Value.Closure 
	| Value.Object 
	| Value.Infix 
	| Value.Forward 
	| Value.Block    -> sexpr_block ~depth fmt r
	| Value.Abstract -> sexpr_abstract ~depth fmt r
	| Value.Custom   -> (
	    match Value.custom_value r with
	      | Value.Custom_nativeint ni -> sexpr_nativeint fmt ni
	      | Value.Custom_int32 i32    -> sexpr_int32 fmt i32
	      | Value.Custom_int64 i64    -> sexpr_int64 fmt i64
	      | _                         -> sexpr_custom ~depth fmt r )
	| Value.Double_array -> sexpr_double_array ~depth fmt r
	| Value.Unaligned
	| Value.Out_of_heap  -> sexpr_addr fmt r
	| Value.Double       -> sexpr_float fmt r
	| Value.Int          -> sexpr_int fmt r
	| Value.String       -> sexpr_string ~depth fmt r
  in

  let values = "DUMP" in
  let r = Value.repr o in
    pp_open_vbox fmt 0;
    sexpr_open fmt values;
    Queue.push r queue;
    while not (Queue.is_empty queue) do
      let r = Queue.pop queue in
	sexpr_sep fmt ();
	sexpr_value ~depth:0 fmt r
    done;
    sexpr_close fmt ();
    pp_close_box fmt ()
