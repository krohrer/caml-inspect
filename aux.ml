(* Kaspar Rohrer, Tue Apr 20 18:57:21 CEST 2010 *)

let with_file_out_channel filename f = 
  let outc = open_out filename in
    try
      f outc;
      flush outc;
      close_out outc
    with
      | e ->
	  close_out outc;
	  raise e

let with_buffer n =
  let b = Buffer.create n in
    ( fun f ->
	Buffer.clear b;
	f b;
	Buffer.contents b
    )

let (|>>) a b = b a
