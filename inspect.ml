(* Kaspar Rohrer, Thu Apr  8 02:24:17 CEST 2010 *)

open Format
open Obj

let (>>>) x f = f x
let ($) f x = f x

module ValueRepr =
struct
  type t = Obj.t
  let equal = (==)
  let hash = Hashtbl.hash
end

module HT = Hashtbl.Make(ValueRepr)

let string_with_buffer n =
  let b = Buffer.create n in
    ( fun f ->
	Buffer.clear b;
	f b;
	Buffer.contents b
    )

let addr r =
  let h = Nativeint.of_int ((magic r : int) land max_int) in
    Nativeint.add h h
      (* according to ${OCAMLSRC}/byterun/mlvalues.h, integers always
	 have the LSB set, and only the upper (Sys.word_size - 1) bits
	 are used to represent the number. Which means that the actual
	 number is the bits of the value right-shifted by 1. If we
	 want to print the bit-pattern (or address) of the value, we
	 therefore have to make sure that the lower bit is one to make
	 a proper int of it and then multiply by 2. *)

(*------------------------------------*)

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

module TagType =
struct
  type t = tag
  let compare = compare
end

module Tags =
struct
  include Set.Make(TagType)

  let of_list tlist =
    List.fold_left (fun s t -> add t s) empty tlist

  let all =
    of_list [
      Lazy;
      Closure;
      Object;
      Infix;
      Forward;
      Block;
      Abstract;
      String;
      Double;
      Double_array;
      Custom;
      Int;
      Out_of_heap;
      Unaligned;
    ]
end

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

let value_in_heap r =
  let x = tag r in
    not (x = int_tag || x = out_of_heap_tag || x = unaligned_tag)

let value_size r =
  if value_in_heap r then size r else 0

let value_mnemonic ?t r = 
  let t = match t with None -> value_tag r | Some x -> x in
    match t with
      | Lazy -> "LAZY"
      | Closure -> "CLOS"
      | Object -> "OBJ"
      | Infix -> "INFX"
      | Forward -> "FWD"
      | Block -> sprintf "BL%d" (tag r)
      | Abstract -> "ABST"
      | String -> "STR"
      | Double -> "DBL"
      | Double_array -> "DBLA"
      | Custom -> "CUST"
      | Int -> "INT"
      | Out_of_heap -> "OADR"
      | Unaligned -> "UADR"

let value_mnemonic_unknown =
  "????"

(*----------------------------------------------------------------------------*)

type dot_attrs = (string * string) list

class type dot_context =
object
  method graph_attrs : dot_attrs
  method all_nodes_attrs : dot_attrs
  method all_edges_attrs : dot_attrs
  method node_attrs : ?root:bool -> label:string -> Obj.t -> dot_attrs
  method edge_attrs : src:Obj.t -> field:int -> dst:Obj.t -> dot_attrs

  method should_expand_node : Obj.t -> bool
  method should_follow_edge : src:Obj.t -> field:int -> dst:Obj.t -> bool
  method max_size : int
end

class type dump_context =
object
  method should_expand : tag -> bool
  method max_depth : int
end

(* See http://www.graphviz.org/doc/info/colors.html for more info. *)

let attrs_colorscheme_for_value ?(k=2.) ?lower ?upper color_scheme n r attrs =
  let f2i = int_of_float and i2f = float_of_int in
  let lower = match lower with None -> 1 | Some l -> l in
  let upper = match upper with None -> n - 1 | Some u -> u in
  let arcsinh x = log (x +. sqrt (x*.x +. 1.)) in
  let x = i2f $ value_size r in
  let y = f2i $ arcsinh ( x /. k ) in
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
  match value_tag r with
    | Infix
    | Forward ->
	(* attrs_colorscheme_for_value "purples" 9 r attrs *)
	attrs_colorscheme_for_value ~lower:2 "ylorbr" 3 r attrs

    | Lazy
    | Closure ->
	attrs_colorscheme_for_value ~k:1.2 ~lower:2 "rdpu" 9 r attrs

    | Object ->
	attrs_colorscheme_for_value ~k:1.2 ~lower:4 "purples" 9 r attrs

    | Block ->
	attrs_colorscheme_for_value ~lower:2 "blues" 9 r attrs

    | Int
    | String
    | Double
    | Double_array ->
  	attrs_colorscheme_for_value ~lower:2 "bugn" 9 r attrs

    | Out_of_heap
    | Unaligned
    | Abstract
    | Custom ->
	attrs_colorscheme_for_value ~k:1.0 ~lower:5 "reds" 9 r attrs

let default_dot_context : dot_context =
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
    let attrs = 
      if root then [ "penwidth", "4.0" ] else []
    in
      ("label", label) :: attrs >>> attrs_for_value r

  method edge_attrs ~src ~field ~dst =
    [ "label", string_of_int field ]

  method should_expand_node r = true
  method should_follow_edge ~src ~field ~dst = true
  method max_size = 20
end

let default_dump_context : dump_context =
object
  method should_expand t = true
  method max_depth = 20
end

(*----------------------------------------------------------------------------*)

let rec dump ?context o =
  dump_with_formatter ?context std_formatter (repr o)

and dump_to_channel ?context c o =
  dump_with_formatter ?context (formatter_of_out_channel c) (repr o)

and dump_to_buffer ?context b o =
  dump_with_formatter ?context (formatter_of_buffer b) (repr o)

and dump_to_string ?context o =
  let b = Buffer.create 128 in
    dump_to_buffer ?context b o;
    Buffer.contents b

and dump_with_formatter ?(context=default_dump_context) fmt o =
  let queue = Queue.create () in
  let indentation_for_string id = 3 (* String.length id + 2 *) in

  let rec value2id = HT.create 31337
  and id_of_value r =
    try
      id_find r
    with Not_found -> (
      let tid = value_mnemonic r in
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

  and sexpr_opaque fmt r t =
    sexpr_open fmt (value_mnemonic ~t r);
    sexpr_sep fmt ();
    sexpr_int fmt (size r);
    sexpr_close fmt ()

  and sexpr_mnemonic fmt r t =
    pp_print_string fmt (value_mnemonic ~t r)
  in

  let rec sexpr_value ~depth fmt r =
    let t = value_tag r in
    let expand = context#should_expand t in
      if not expand then
	sexpr_mnemonic fmt r t
      else
	match t with
	  | Lazy 
	  | Closure 
	  | Object 
	  | Infix 
	  | Forward 
	  | Block ->
	      sexpr_block ~depth fmt r t sexpr_block_body
	  | Abstract
	  | Custom ->
	      sexpr_opaque fmt r t
	  | Double_array ->
	      sexpr_block ~depth fmt r t sexpr_double_array_body
	  | Unaligned | Out_of_heap ->
	      sexpr_addr fmt (addr r)
	  | Double ->
	      sexpr_float fmt (magic r : float)
	  | Int ->
	      sexpr_int fmt (magic r : int)
	  | String ->
	      sexpr_string fmt (magic r : string)

  and sexpr_block ~depth fmt r t body =
    try
      sexpr_ref fmt (id_find r)
    with Not_found -> (
      let id = id_of_value r in
	if depth <= context#max_depth then (
	  sexpr_open fmt id;
	  body ~depth fmt r t;
	  sexpr_close fmt ()
	) else if context#max_depth > 0 then (
	  (* Postpone *)
	  sexpr_ref fmt id;
	  Queue.push r queue
	)
	else (
	  (* Cant print with max_depth < 0 *)
	)
    )

  and sexpr_block_body ~depth fmt r _ =
    assert (tag r < no_scan_tag);
    let n = size r in
      for i = 0 to n - 1 do
	sexpr_sep fmt ();
	sexpr_value ~depth:(depth + 1) fmt (field r i)
      done

  and sexpr_double_array_body ~depth fmt r _ =
    assert (tag r = double_array_tag);
    let a : float array = magic r in
    let n = Array.length a in
      for i = 0 to n - 1 do
	sexpr_sep fmt ();
	sexpr_float fmt a.(i)
      done
  in

  let values = "DUMP" in
  let r = repr o in
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

(*----------------------------------------------------------------------------*)

let rec dot ?context o =
  dot_with_formatter ?context std_formatter (repr o)

and dot_osx ?context o =
  let basename = Filename.temp_file "camldump" "." in
  let pr = "dot" in
  let format = "pdf" in
  let dotfile = basename ^ "dot" in
  let outfile = basename ^ format in
    dot_to_file ?context dotfile o;
    let dotcmd = sprintf "%s -T%s -o %S %S" pr format outfile dotfile in
    let outcmd = sprintf "open %S" outfile in
    Sys.command dotcmd == 0 &&
      Sys.command outcmd == 0

and dot_to_file ?context path o =
  let oc = open_out path in
    try
      let fmt = formatter_of_out_channel oc in
	dot_with_formatter ?context fmt (repr o);
	flush oc;
	close_out oc
    with
      | _ -> close_out oc

and dot_with_formatter ?(context=default_dot_context) fmt r =
  let queue = Queue.create () in
  let strbuf = string_with_buffer 80 in

  let rec value2id = HT.create 31337
  and id_of_value r =
    try id_find r with Not_found -> (
      let t = value_tag r in
      let id = sprintf "%s_%d" (value_mnemonic ~t r) (HT.length value2id) in
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

  let value_descr ?(long=false) r t =
    let string_ellipsis = "[..]" in
    let string_max_length = 8 in
    let string_cutoff = 4 in
      match value_tag r with
	| Double as t ->
	    let d : float = magic r in
	      if long then
		sprintf "%s %g" (value_mnemonic ~t r) d
	      else
		string_of_float d
	| Int as t ->
	    let i : int = magic r in
	      if long then
		sprintf "%s %d" (value_mnemonic ~t r) i
	      else
		string_of_int i
	| Out_of_heap
	| Unaligned as t ->
	    if long then
	      sprintf "%s 0x%nX" (value_mnemonic ~t r) (addr r)
	    else
	      sprintf "0x%nX" (addr r)
	| Lazy 
	| Forward
	| Custom 
	| Block 
	| Closure 
	| Object 
	| Infix 
	| Abstract as t ->
	    sprintf "%s #%d" (value_mnemonic ~t r) (size r)
	| Double_array ->
	    let a : float array = magic r in
	      sprintf "%s #%d" (value_mnemonic ~t r) (Array.length a)
	| String ->
	    let s : string = magic r in
	    let l = String.length s in
	    let s' =
	      if l > string_max_length then
		String.sub s 0 string_cutoff ^ string_ellipsis
	      else
		s
	    in
	      sprintf "%S#%d" s' l
  in

  let value_to_label_and_links id r =
    let t = value_tag r in
    let max_size = context#max_size in
    let expand = context#should_expand_node r in
    let bstr b s = Buffer.add_string b s
    and bsep b () = Buffer.add_string b "| "
    and brest b () = Buffer.add_string b "..."
    in
    let bprint b =
      Buffer.add_string b $ value_descr ~long:true r t;
      match t with
	| _ when tag r < no_scan_tag && expand ->
	    let n = size r in
	    let n' = min max_size n in
	    let cutoff = if n' = max_size then n' - 1 else max_int in
	      for i = 0 to n' - 1 do
		bsep b ();
		if i = cutoff then
		  brest b ()
		else
		  let f = field r i in
		  let x = value_tag f in
		    bstr b $ value_descr f x
	      done
	| Double_array when expand ->
	    let a : float array = magic r in
	    let n = Array.length a in
	    let n' = min max_size n in
	    let cutoff = if n' = max_size then n' - 1 else max_int in
	      for i = 0 to n' - 1 do
		bsep b ();
		if i = cutoff then
		  brest b ()
		else
		  bstr b $ string_of_float a.(i)
	      done
	| Custom | Abstract when expand ->
	    let n = size r in
	    let n' = min max_size n in
	    let cutoff = if n' = max_size then n' - 1 else max_int in
	      for i = 0 to n' - 1 do
		bsep b ();
		if i = cutoff then
		  brest b ()
		else
		  bstr b value_mnemonic_unknown
	      done
	| String when expand ->
	    let lsub = 16 in
	    let s : string = magic r in
	    let l = String.length s in
	    let n' = min max_size ((l + lsub - 1) / lsub) in
	    let cutoff = if n' = max_size then n' - 1 else max_int in
	      for i = 0 to n' - 1 do
		bsep b ();
		if i = cutoff then
		  brest b ()
		else
		  let isub = i * 16 in
		  let len = min (String.length s - isub) lsub in
		    bprintf b "%S" $ String.sub s isub len
	      done
	| _ ->
	    ()
    in
    let links =
      if tag r < no_scan_tag && expand then
	let rl = ref [] in
	let n = size r in
	  for i = 0 to n - 1 do
	    let f = field r i in
	      if value_in_heap f && context#should_follow_edge ~src:r ~field:i ~dst:f then (
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

let wobytes = Sys.word_size / 8
let hdbytes = wobytes

let heap_size ?(tags=Tags.all) ?(follow=Tags.all) o =
  let inspected = HT.create 31337 in
  let candidates = Stack.create () in
  let add_candidate r =
    if HT.mem inspected r then
      ()
    else
      Stack.push r candidates
  in
  let has_candidates () = 
    not (Stack.is_empty candidates)
  in
  let next_candidate () =
    let r = Stack.pop candidates in
      HT.add inspected r ();
      r
  in
  let bytes = ref 0 in
  let add_bytes d =
    bytes := !bytes + d
  in
    Stack.push (repr o) candidates;
    while has_candidates () do
      let r = next_candidate () in
      let t = value_tag r in
	match t with
	  | Lazy | Closure | Object | Infix | Forward | Block as x ->
	      let n = size r in
		( if Tags.mem x tags then
		    add_bytes (hdbytes + n*wobytes);
		  if Tags.mem x follow then
		    for i = 0 to n - 1 do
		      add_candidate (field r i)
		    done
		)
	  | Abstract | String | Double | Double_array | Custom as x->
	      let n = size r in
		( if Tags.mem x tags then
		    add_bytes (hdbytes * n*wobytes)
		)
	  | Int | Out_of_heap | Unaligned ->
	      ()
    done;
    !bytes

(*----------------------------------------------------------------------------*)

let rec test_data () =
  let rec l = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: l in
  let rec drop l i =
    if i = 0 then
      l
    else
      drop (List.tl l) (i - 1)
  in
  let rec f x =
    l
  and g y =
    f (y :: l)
  in
  let o = object
    val brog = 4
    val brag = 51251
    method blah = 3 
    method foo () a = a
  end in
  let data = 
    ([|1|], l, (1,2), [|3; 4|], flush, 1.0, [|2.0; 3.0|],
     String.make 1000000 'a',
     (Tags.all, Tags.remove Closure Tags.all),
     ("Hello world", lazy (3 + 5)), g, f, let s = "STRING" in (s, "STRING", s),
     Array.init 20 (drop l),
     stdout, printf, (o, default_dump_context, default_dot_context),
     [String.make 10 'a'; String.make 100 'a'; String.make 1000 'a'; String.make 10000000 'a'],
    [Array.make 1 1; Array.make 4 4; Array.make 16 16; Array.make 64 64; Array.make 256 256;
     Array.make 1024 1024; Array.make 1000000 0],
    [Array.make 1 1.; Array.make 4 4.; Array.make 16 16.; Array.make 64 64.; Array.make 256 256.;
     Array.make 1024 1024.; Array.make 1000000 0.]
    )
  in
    repr data

let test () = dot_osx (test_data ());

(*----------------------------------------------------------------------------*)

