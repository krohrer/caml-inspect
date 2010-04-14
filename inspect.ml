(* Kaspar Rohrer, Thu Apr  8 02:24:17 CEST 2010 *)

(*----------------------------------------------------------------------------*)

let dump a = Dump_sexpr.dump a
let dot a = ignore (Dump_dot.dump_command a)

(*----------------------------------------------------------------------------*)

module HT = Hashtbl.Make(Value)

let count_heap_words_and_objects o =
  let inspected = HT.create 31337 in
  let candidates = Stack.create () in
  let words = ref 0 in
    Stack.push (Obj.repr o) candidates;
    while not (Stack.is_empty candidates) do
      let r = Stack.pop candidates in
	HT.add inspected r ();
	words := !words + Value.heap_words r;
	if Obj.tag r < Obj.no_scan_tag then
	  for i = 0 to Obj.size r - 1 do
	    if not (HT.mem inspected r) then
	      Stack.push r candidates
	  done
    done;
    !words, HT.length inspected

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
     ("Hello world", lazy (3 + 5)), g, f, let s = "STRING" in (s, "STRING", s),
     Array.init 20 (drop l),
     stdout, Printf.printf, (o, Dump_dot.default_context, Dump_sexpr.default_context),
     [String.make 10 'a'; String.make 100 'a'; String.make 1000 'a'; String.make 10000000 'a'],
    [Array.make 1 1; Array.make 4 4; Array.make 16 16; Array.make 64 64; Array.make 256 256;
     Array.make 1024 1024; Array.make 1000000 0],
    [Array.make 1 1.; Array.make 4 4.; Array.make 16 16.; Array.make 64 64.; Array.make 256 256.;
     Array.make 1024 1024.; Array.make 1000000 0.]
    )
  in
    Obj.repr data

(*----------------------------------------------------------------------------*)

