(* Kaspar Rohrer, Wed Apr 14 13:46:21 CEST 2010 *)

open Format

module HT = Hashtbl.Make(Value)

class type context =
object
  method should_expand : Value.t -> bool
  method nesting : int
end

let make_context ?(nesting=30) () =
object
  method should_expand v = true
  method nesting = nesting
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
      let id = sprintf "%s/%X" tid n in
	HT.add value2id r id;
	id
    )
  and id_find r =
    HT.find value2id r
  in

  let rec sexpr_open fmt id =
    fprintf fmt "@[<hv %d>(%s" (indentation_for_string id) id

  and sexpr_close fmt () =
    fprintf fmt ")@]"

  and sexpr_sep fmt () =
    fprintf fmt "@ "

  and sexpr_ref fmt id =
    fprintf fmt "@@%s" id

  and sexpr_string fmt s =
    fprintf fmt "%S" s

  and sexpr_float fmt f =
    fprintf fmt "%f" f

  and sexpr_int fmt i =
    fprintf fmt "%d" i

  and sexpr_addr fmt a =
    fprintf fmt "0x%nX" a

  and sexpr_abstract fmt r =
    sexpr_open fmt (Value.mnemonic r);
    sexpr_sep fmt ();
    fprintf fmt ":SIZE %d" (Value.heap_words r);
    sexpr_close fmt ()

  and sexpr_custom fmt r =
    match Value.custom_value r with
      | Value.Custom_nativeint n ->
	  fprintf fmt "%nd" n
      | Value.Custom_int32 i ->
	  fprintf fmt "%ld" i
      | Value.Custom_int64 i ->
	  fprintf fmt "%Ld" i
      | _ ->
	  sexpr_open fmt (Value.mnemonic r);
	  sexpr_sep fmt ();
	  fprintf fmt ":ID %S" (Value.custom_identifier r);
	  sexpr_sep fmt ();
	  fprintf fmt ":SIZE %d" (Value.heap_words r);
	  sexpr_sep fmt ();
	  fprintf fmt ":OPS %s" (Value.custom_ops_info r);
	  sexpr_close fmt ()

  and sexpr_mnemonic fmt r =
    pp_print_string fmt (Value.mnemonic r)
  in

  let rec sexpr_value ~depth fmt r =
    let t = Value.tag r in
    let expand = context#should_expand r in
      if not expand then
	sexpr_mnemonic fmt r
      else
	match t with
	  | Value.Lazy 
	  | Value.Closure 
	  | Value.Object 
	  | Value.Infix 
	  | Value.Forward 
	  | Value.Block ->
	      sexpr_block ~depth fmt r sexpr_block_body
	  | Value.Abstract ->
	      sexpr_abstract fmt r
	  | Value.Custom ->
	      sexpr_custom fmt r
	  | Value.Double_array ->
	      sexpr_block ~depth fmt r sexpr_double_array_body
	  | Value.Unaligned
	  | Value.Out_of_heap ->
	      sexpr_addr fmt (Value.bits r)
	  | Value.Double ->
	      sexpr_float fmt (Obj.magic r : float)
	  | Value.Int ->
	      sexpr_int fmt (Obj.magic r : int)
	  | Value.String ->
	      sexpr_string fmt (Obj.magic r : string)

  and sexpr_block ~depth fmt r body =
    try
      sexpr_ref fmt (id_find r)
    with Not_found -> (
      let id = id_of_value r in
	if depth <= context#nesting then (
	  sexpr_open fmt id;
	  body ~depth fmt r;
	  sexpr_close fmt ()
	) else if context#nesting > 0 then (
	  (* Postpone *)
	  sexpr_ref fmt id;
	  Queue.push r queue
	)
	else (
	  (* Cant print with nesting < 0 *)
	)
    )

  and sexpr_block_body ~depth fmt r =
    assert (Obj.tag r < Obj.no_scan_tag);
    let n = Obj.size r in
      for i = 0 to n - 1 do
	sexpr_sep fmt ();
	sexpr_value ~depth:(depth + 1) fmt (Obj.field r i)
      done

  and sexpr_double_array_body ~depth fmt r =
    assert (Obj.tag r = Obj.double_array_tag);
    let a : float array = Obj.magic r in
    let n = Array.length a in
      for i = 0 to n - 1 do
	sexpr_sep fmt ();
	sexpr_float fmt a.(i)
      done
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
