(* Kaspar Rohrer, Wed Apr 14 01:57:49 CEST 2010 *)

module HT = Hashtbl.Make(Value)

let string_with_buffer n =
  let b = Buffer.create n in
    ( fun f ->
	Buffer.clear b;
	f b;
	Buffer.contents b
    )

open Format

let (|>>) a b = b a

type dot_attrs = (string * string) list

class type context =
object
  method graph_attrs : dot_attrs
  method all_nodes_attrs : dot_attrs
  method all_edges_attrs : dot_attrs
  method node_attrs : ?root:bool -> label:string -> Obj.t -> dot_attrs
  method edge_attrs : src:Obj.t -> field:int -> dst:Obj.t -> dot_attrs

  method should_expand_node : Obj.t -> bool
  method should_follow_edge : src:Obj.t -> field:int -> dst:Obj.t -> bool
  method max_fields : int
end

(*----------------------------------------------------------------------------*)

(* See http://www.graphviz.org/doc/info/colors.html for more info. *)

let attrs_colorscheme_for_value ?(k=2.) ?lower ?upper color_scheme n r attrs =
  let f2i = int_of_float and i2f = float_of_int in
  let lower = match lower with None -> 1 | Some l -> l in
  let upper = match upper with None -> n - 1 | Some u -> u in
    (* Arcsin seems like a good choice here, it starts of almost
       linear, but the derivative seems to decay at an exponential
       rate. k adjusts the shape in that approximates the distance
       between the first two colors. *)
  let arcsinh x = log (x +. sqrt (x*.x +. 1.)) in
  let x = i2f (Value.heap_words r) in
  let y = f2i (arcsinh ( x /. k )) in
  let i = min upper (y + lower) in
  let attrs = 
    ("colorscheme", color_scheme ^ string_of_int n)
    :: ("fillcolor", string_of_int i)
    :: ("color", string_of_int n)
    :: attrs
  in
    if i = n then
      ("fontcolor", "white") :: attrs
    else
      attrs

let attrs_for_value r attrs =
  match Value.tag r with
    | Value.Infix
    | Value.Forward ->
	(* attrs_colorscheme_for_value "purples" 9 r attrs *)
	attrs_colorscheme_for_value ~lower:2 "ylorbr" 3 r attrs

    | Value.Lazy
    | Value.Closure ->
	attrs_colorscheme_for_value ~k:1.2 ~lower:2 "rdpu" 9 r attrs

    | Value.Object ->
	attrs_colorscheme_for_value ~k:1.2 ~lower:4 "purples" 9 r attrs

    | Value.Block ->
	attrs_colorscheme_for_value ~lower:2 "blues" 9 r attrs

    | Value.Int
    | Value.String
    | Value.Double
    | Value.Double_array ->
  	attrs_colorscheme_for_value ~lower:2 "bugn" 9 r attrs

    | Value.Custom when Value.custom_is_int r -> (
	match Value.custom_value r with
	  | Value.Custom_nativeint _
	  | Value.Custom_int32 _
	  | Value.Custom_int64 _ ->
  	      attrs_colorscheme_for_value ~lower:2 "bugn" 9 r attrs
	  | _ ->
	      attrs_colorscheme_for_value ~k:1.0 ~lower:5 "reds" 9 r attrs
      )

    | Value.Out_of_heap
    | Value.Unaligned
    | Value.Abstract
    | Value.Custom ->
	attrs_colorscheme_for_value ~k:1.0 ~lower:5 "reds" 9 r attrs

let make_context ?(max_fields=5) () =
object
  method graph_attrs =
    [
      "rankdir", "LR";
      "splines", "true";
      "overlap", "false";
      "sep", "0.1"
    ]

  method all_nodes_attrs =
    [
      "shape", "record";
      "penwidth", "2.0";
      "style", "rounded, filled"
    ]

  method all_edges_attrs =
    [
      "dir", "both";
      "arrowtail", "odot"
    ]

  method node_attrs ?(root=false) ~label r =
    let attrs_for_root attrs = 
      if root then
	(* ("fontcolor", "#ff0000") :: *) ("penwidth", "8.0") :: attrs
      else
	attrs
    in
      ["label", label] |>> attrs_for_value r |>> attrs_for_root

  method edge_attrs ~src ~field ~dst =
    [ "label", string_of_int field ]

  method should_expand_node r = true
  method should_follow_edge ~src ~field ~dst =
    match Value.tag dst with
      | Value.Double ->
	  false
      | Value.Custom -> (
	  match Value.custom_value dst with
	    | Value.Custom_nativeint _
	    | Value.Custom_int32 _
	    | Value.Custom_int64 _ ->
		false
	    | _ ->
		true
	)
      | _ ->
	  true

  method max_fields = max_fields
end

let default_context = make_context ()

(*----------------------------------------------------------------------------*)

let dump_with_formatter ?(context=default_context) fmt o =
  let queue = Queue.create () in
  let strbuf = string_with_buffer 80 in

  let rec value2id = HT.create 31337
  and id_of_value r =
    try id_find r with Not_found -> (
      let id = sprintf "%s_%d" (Value.mnemonic r) (HT.length value2id) in
	HT.add value2id r id;
	id
    )
  and id_find r =
    HT.find value2id r
  in

  let node_open fmt id =
    fprintf fmt "@[<2>%s@ [" id

  and node_close fmt () =
    fprintf fmt "];@]@,"

  and link_open fmt id i fid =
    let src = id in
    let dst = fid in
      fprintf fmt "@[<2>%s ->@ %s@ [" src dst

  and link_close fmt () =
    fprintf fmt "];@]@,"

  and attr_open fmt name =
    fprintf fmt "@[<h>%s = " name

  and attr_close fmt () =
    fprintf fmt ",@]@ "
  in

  let rec node_one fmt id attrs =
    node_open fmt id;
    attr_list fmt attrs;
    node_close fmt ()

  and link_one fmt id i fid attrs =
    link_open fmt id i fid;
    attr_list fmt attrs;
    link_close fmt ()

  and attr_one fmt name value =
    attr_open fmt name;
    fprintf fmt "%S" value;
    attr_close fmt ()

  and attr_list fmt attrs =
      (* The list has to be reversed because of the way Graphviz handles
	 duplicate attributes. *)
      List.iter (fun (k,v) -> attr_one fmt k v) (List.rev attrs)
  in

  let value_to_label_and_links id r =
    let max_fields = context#max_fields in
    let expand = context#should_expand_node r in
    let bstr b s = Buffer.add_string b s
    and bsep b () = Buffer.add_string b "| "
    and brest b () = Buffer.add_string b "..."
    in
    let bprint b =
      Buffer.add_string b (Value.description r);
      match Value.tag r with
	| _ when Obj.tag r < Obj.no_scan_tag && expand ->
	    let n = Obj.size r in
	    let n' = min max_fields n in
	    let cutoff = if n' = max_fields then n' - 1 else max_int in
	      for i = 0 to n' - 1 do
		bsep b ();
		if i = cutoff then
		  brest b ()
		else
		  let f = Obj.field r i in
		    bstr b (Value.abbrev f)
	      done
	| Value.Double_array when expand ->
	    let a : float array = Obj.magic r in
	    let n = Array.length a in
	    let n' = min max_fields n in
	    let cutoff = if n' = max_fields then n' - 1 else max_int in
	      for i = 0 to n' - 1 do
		bsep b ();
		if i = cutoff then
		  brest b ()
		else
		  bstr b (string_of_float a.(i))
	      done
	| Value.Custom | Value.Abstract when expand ->
	    let n = Obj.size r in
	    let n' = min max_fields n in
	    let cutoff = if n' = max_fields then n' - 1 else max_int in
	      for i = 0 to n' - 1 do
		bsep b ();
		if i = cutoff then
		  brest b ()
		else
		  bstr b Value.mnemonic_unknown
	      done
	| Value.String when expand ->
	    let lsub = 16 in
	    let s : string = Obj.magic r in
	    let l = String.length s in
	    let n' = min max_fields ((l + lsub - 1) / lsub) in
	    let cutoff = if n' = max_fields then n' - 1 else max_int in
	      for i = 0 to n' - 1 do
		bsep b ();
		if i = cutoff then
		  brest b ()
		else
		  let isub = i * 16 in
		  let len = min (String.length s - isub) lsub in
		    bprintf b "%S" (String.sub s isub len)
	      done
	| _ ->
	    ()
    in
    let links =
      if Obj.tag r < Obj.no_scan_tag && expand then
	let rl = ref [] in
	let n = Obj.size r in
	  for i = 0 to n - 1 do
	    let f = Obj.field r i in
	      if Value.is_in_heap f && context#should_follow_edge ~src:r ~field:i ~dst:f then (
		let _ = try ignore (id_find f) with Not_found -> Queue.push f queue in
		  rl := (r, i, f) :: !rl
	      )
	  done;
	  !rl
      else
	[]
    in
      strbuf bprint, links
  in

  let rec value_one ?(root=false) fmt id r =
    let label, links = value_to_label_and_links id r in
    let node_attrs = context#node_attrs ~root ~label r in
    let aux (src, i, dst) = 
      let edge_attrs = context#edge_attrs ~src ~field:i ~dst in
      let id = id_of_value src and fid = id_of_value dst in
	link_one fmt id i fid edge_attrs
    in
      node_one fmt id node_attrs;
      List.iter aux links
  in
  let r = Obj.repr o in
  let root_id = id_of_value r in
    fprintf fmt "@[<v>@[<v 2>digraph {@,";
    node_one fmt "graph" (("root", root_id) :: context#graph_attrs);
    node_one fmt "node" context#all_nodes_attrs;
    node_one fmt "edge" context#all_edges_attrs;
    value_one ~root:true fmt root_id r;
    while not (Queue.is_empty queue) do
      let r = Queue.pop queue in
	value_one fmt (id_of_value r) r
    done;
    fprintf fmt "@]@,}@]";
    pp_print_newline fmt ()

(*----------------------------------------------------------------------------*)
