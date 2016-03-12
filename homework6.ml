(* 

HOMEWORK 6

Name: Nitya Dhanushkodi

Email: nitya.dhanushkodi@students.olin.edu

Remarks, if any:

*)


(*
 *
 * Please fill in this file with your solutions and submit it
 *
 * The functions below are stubs that you should replace with your
 * own implementation.
 *
 * Always make sure you can #use this file before submitting it.
 * Do that in a _fresh_ OCaml shell 
 * It has to load without any errors.
 *
 *)




(* 
 * String <-> characters utility functions:
 *
 *   explode : string -> string list
 *      returns the list of characters making up a string
 *
 *)

let explode str = 
  let rec acc index result = 
    if (index<0) then result
    else acc (index-1) ((String.sub str index 1)::result) in
  acc (String.length(str)-1) []


(*
 * Type for deterministic Turing machines
 *
 * Parameterized by type for states
 *
 *)

type symbol = string

type 'a tm = { states : 'a list;
	       input_alphabet : symbol list;
	       tape_alphabet : symbol list;
	       left_marker : symbol;
	       blank : symbol;
	       delta : ('a * symbol) -> ('a * symbol * int);   (* 0 = Left, 1 = Right *)
	       start : 'a;
	       accept : 'a;
	       reject : 'a }

type 'a config = { state : 'a;
		   before: symbol list;
		   after: symbol list }


(*
 *   Code to run a string tm machine
 *
 *)

let run m w = 

  let printConfig m config value = 
    let mw = 
      List.fold_right (fun a r -> max (String.length a) r) m.states 0 in
    let _ = 
      print_string (String.sub (config.state^(String.make mw ' ')) 0 mw) in
    let print_syms = List.iter (Printf.printf " %s ")  in
    let _ = print_string "  "  in
    let _ = print_syms config.before  in
    let _ = (match config.after with 
             | [] -> Printf.printf "[%s]" m.blank
	     | a::v' -> let _ = Printf.printf "[%s]" a  in
	       print_syms v') in
    let _ = print_newline ()  in
    value  in

  let acceptConfig m config = (config.state=m.accept) in

  let rejectConfig m config = (config.state=m.reject) in

  let haltConfig m c = (acceptConfig m c) || (rejectConfig m c) in

  let startConfig m w = 
    { state=m.start;before = [];after = m.left_marker::(explode w)} in

  let rec last u = 
    match u with
    | [] -> failwith "Moving Left from leftmost tape position"
    | [a] -> ([],a)
    | x::xs -> let (u',r) = last xs  in (x::u',r)   in

  let step m config = 
    if (haltConfig m config) then config
    else let (a,v') = match config.after with
                      | [] -> (m.blank,[])
		      | a::v' -> (a,v')  in
         let (q',b,dir) = m.delta(config.state,a) in
	 if dir = 0  (* left *)
	 then let (u',c) = last config.before in 
  	      {state=q';before=u';after=c::b::v'}
	 else {state=q';before=config.before@[b];after=v'} in

  let rec loop c = 
    let _ = printConfig m c c in
    if  (acceptConfig m c) then true
    else if (rejectConfig m c) then false
    else loop (step m c)  in

  loop (startConfig m w)


let rec pairs xs ys =
  List.fold_right (fun x r -> (List.map (fun y -> (x,y)) ys)@r) xs []




(* QUESTION 1 *)

(*Code a function triples of type 'a list -> 'b list -> 'c list -> ('a * 'b * 'c) list 
where (triples xs ys zs) returns the list of all triples (a,b,c) where a is an element of 
xs, b is an element of ys, and c is an element of zs. Order is not important.

# triples [] [] [];;
- : ('a * 'b * 'c) list = []
# triples [] ["a";"b"] [100;101];;
- : ('a * string * int) list = []
# triples [1;2] [] [100;101];;
- : (int * 'a * int) list = []
# triples [1;2] ["a";"b"] [];;
- : (int * string * 'a) list = []
# triples [1] ["a"] [100];;
- : (int * string * int) list = [(1, "a", 100)]
# triples [1;2;3] ["a";"b"] [100;101;102];;
- : (int * string * int) list =
[(1, "a", 100); (1, "a", 101); (1, "a", 102); (1, "b", 100); (1, "b", 101);
 (1, "b", 102); (2, "a", 100); (2, "a", 101); (2, "a", 102); (2, "b", 100);
 (2, "b", 101); (2, "b", 102); (3, "a", 100); (3, "a", 101); (3, "a", 102);
 (3, "b", 100); (3, "b", 101); (3, "b", 102)]

*)
let triples xs ys zs = 
  List.fold_right(fun (x,y) r -> (List.map(fun z -> (x,y,z)) zs)@r) (pairs xs ys) []

(*

Code a function quads of type 'a list -> 'b list -> 'c list -> 'd list -> ('a * 'b * 'c * 'd) list where (quads xs ys zs ws) returns the list of all tuples (a,b,c,d) where a is an element of xs, b is an element of ys, c is an element of zs, and d is an element of ws. Order is not important.

# quads [] [] [] [];;
- : ('a * 'b * 'c * 'd) list = []
# quads [] ["a";"b"] [100;101] [true;false];;
- : ('a * string * int * bool) list = []
# quads [1;2] [] [100;101] [true;false];;
- : (int * 'a * int * bool) list = []
# quads [1;2] ["a";"b"] [] [true;false];;
- : (int * string * 'a * bool) list = []
# quads [1;2] ["a";"b"] [100;101] [];;
- : (int * string * int * 'a) list = []
# quads [1] ["a"] [100] [true];;
- : (int * string * int * bool) list = [(1, "a", 100, true)]
# quads [1;2] ["a";"b"] [100;101] [true;false];;
- : (int * string * int * bool) list =
[(1, "a", 100, true); (1, "a", 100, false); (1, "a", 101, true);
 (1, "a", 101, false); (1, "b", 100, true); (1, "b", 100, false);
 (1, "b", 101, true); (1, "b", 101, false); (2, "a", 100, true);
 (2, "a", 100, false); (2, "a", 101, true); (2, "a", 101, false);
 (2, "b", 100, true); (2, "b", 100, false); (2, "b", 101, true);
 (2, "b", 101, false)]

*)
let quads xs ys zs ws = 
  List.fold_right(fun (x,y,z) r -> (List.map(fun w -> (x,y,z,w)) ws)@r) (triples xs ys zs) []  


let rec range n = 
  if n<0 then [] else n::(range (n-1))



(* QUESTION 2 *)

(*Code a function transformStates of type 'a list -> ('a -> b) -> 'b list where 
(transformStates states f) returns the result of applying transformation f to every state 
in states.

# let trans (x,y) = x^"/"^(string_of_int y);;
val trans : string * int -> string = 
# trans ("hello",10);;
- : string = "hello/10"

# transformStates [] trans;;
- : string list = []
# transformStates [("a",1);("b",2);("c",3)] trans;;
- : string list = ["a/1"; "b/2"; "c/3"]
*)
let transformStates states f = 
  List.map(fun st -> f st) states

(*Code a function find_original of type 'a list -> ('a -> 'b) -> 'b -> 'a where 
(find_original states f target) returns the state s in states for which (f s) is target. 
In other words, it returns the original state that transforms to target.

# let trans (x,y) = x^"/"^(string_of_int y);;
val trans : string * int -> string = 
# trans ("hello",10);;
- : string = "hello/10"

# find_original [("a",1);("b",2);("c",3)] trans "a/1";;
- : string * int = ("a", 1)
# find_original [("a",1);("b",2);("c",3)] trans "b/2";;
- : string * int = ("b", 2)
# find_original [("a",1);("b",2);("c",3)] trans "c/3";;
- : string * int = ("c", 3)
# find_original [("a",1);("b",2);("c",3)] trans "d/3";;
Exception: Failure "cannot find original value".

*)
let rec find_original states f target = 
  match states with [] -> failwith "cannot find original value"
  | first::rest -> if target = f first then first else find_original rest f target

(*Code a function transformDelta of type 
'a list -> ('a * 'b -> 'a * 'b * int) -> ('a -> 'c) -> ('c * 'b -> 'c * 'b * int) where
 (transformDelta states delta f) takes a a list of states and a Turing machine transition 
 function delta as well as a state transformation function f and returns a new transition 
 function with same transitions as delta except using the transformed states instead of the
  original states. Thus, if delta(p,a) is (q,b,d), then the new transition function takes 
  (f p,a) to (f q,b,d).

Hint: you may find function find_original useful. Just sayin'.

Also: if you get the answer to this one right and you understand what you're doing, then you've understood everything there is to understand about higher-order functions. Kudos.

# let trans (x,y) = x^"/"^(string_of_int y);;
val trans : string * int -> string = 
# trans ("hello",10);;
- : string = "hello/10"

# let delta x = match x with
                | (("a",1),"0") -> (("b",2),"0",0)
                | (("a",1),"1") -> (("c",3),"1",0)
                | (("b",2),"0") -> (("c",3),"0",0)
                | (("b",2),"1") -> (("a",1),"1",0)
                | (("c",3),"0") -> (("a",1),"0",1)
                | (("c",3),"1") -> (("b",2),"1",1)
                | (_,sym) -> (("a",1),sym,1)  ;;
val delta : (string * int) * string -> (string * int) * string * int = 
# delta(("a",1),"0");;
- : (string * int) * string * int = (("b", 2), "0", 0)
# delta(("b",2),"0");;
- : (string * int) * string * int = (("c", 3), "0", 0)
# delta(("c",3),"0");;
- : (string * int) * string * int = (("a", 1), "0", 1)
# let new_delta = transformDelta [("a",1);("b",2);("c",3)] delta trans;;
val new_delta : string * string -> string * string * int = 
# new_delta("a/1","0");;
- : string * string * int = ("b/2", "0", 0)
# new_delta("b/2","0");;
- : string * string * int = ("c/3", "0", 0)
# new_delta("c/3","0");;
- : string * string * int = ("a/1", "0", 1)
*) 
(*let sum x = (fun y -> x+y)
let hi = sum 10*) 
let transformDelta states delta f = 
  fun (st,symbol)-> match delta ((find_original states f st), symbol)  with (x,y,z) -> ((f x), y, z)

(*Code a function transform of type 'a tm -> ('a -> 'b) -> 'b tm where 
(transform m f) returns a Turing machine that acts just like Turing machine m 
except that it uses the states obtained by transforming the states of m by f.

As the example in the introduction shows, you can use transform to transform a Turing 
machine with structured states (i.e., states represented using tuples) into a Turing 
machines with strings as states which you can feed to function run.

For a simple output, run the add1 machine as in the introduction.
*)  
let transform m f = 
  {
    states = transformStates m.states f;
    input_alphabet = m.input_alphabet;
    tape_alphabet = m.tape_alphabet;
    blank = m.blank;
    left_marker = m.left_marker;
    start = f m.start;
    accept = f m.accept;
    reject = f m.reject;
    delta = transformDelta m.states m.delta f

  }




(* 
 * Some sample deterministic Turing machines with structured states
 *
 * anbn is the non-regular language {a^n b^n | n >= 0}
 * add1  accepts strings u#v where v = u+1 in binary
 *
 *)


let anbn = { states = [ ("start",0); 
			("q",1);
			("q",2);
			("q",3);
			("q",4);
			("acc",0);
			("rej",0) ];
	     input_alphabet = ["a";"b"];
	     tape_alphabet = ["a";"b";"X";"/";"|"];
	     blank = "/";
	     left_marker = "|";
	     start = ("start",0);
	     accept = ("acc",0);
	     reject = ("rej",0);
	     delta = (fun inp -> match inp with
	                 | (("start",0), "a") -> (("start",0), "a", 1)
     			 | (("start",0), "b") -> (("q",1), "b", 1)
			 | (("start",0), "|") -> (("start",0), "|", 1)
			 | (("start",0), "/") -> (("q",2), "/", 1)
			 | (("q",1), "b") -> (("q",1), "b", 1)
			 | (("q",1), "/") -> (("q",2), "/", 1)
			 | (("q",2), "|") -> (("q",3), "|", 1)
			 | (("q",2), "a") -> (("q",2), "a", 0)
			 | (("q",2), "b") -> (("q",2), "b", 0)
			 | (("q",2), "X") -> (("q",2), "X", 0)
			 | (("q",2), "/") -> (("q",2), "/", 0)
			 | (("q",3), "X") -> (("q",3), "X", 1)
			 | (("q",3), "/") -> (("acc",0), "/", 1)
			 | (("q",3), "a") -> (("q",4), "X", 1)
			 | (("q",4), "a") -> (("q",4), "a", 1)
			 | (("q",4), "X") -> (("q",4), "X", 1)
			 | (("q",4), "b") -> (("q",2), "X", 1)
			 | (("acc",0), s) -> (("acc",0),s,1)
			 | (_,c) -> (("rej",0),c,1))}


let add1 = 
  { states =    (* spelled out fully so as not to rely on 'triples' *)
[("start", -1, -1); ("start", -1, 0); ("start", -1, 1); ("start", 0, -1);
 ("start", 0, 0); ("start", 0, 1); ("start", 1, -1); ("start", 1, 0);
 ("start", 1, 1); ("check1", -1, -1); ("check1", -1, 0); ("check1", -1, 1);
 ("check1", 0, -1); ("check1", 0, 0); ("check1", 0, 1); ("check1", 1, -1);
 ("check1", 1, 0); ("check1", 1, 1); ("check2", -1, -1); ("check2", -1, 0);
 ("check2", -1, 1); ("check2", 0, -1); ("check2", 0, 0); ("check2", 0, 1);
 ("check2", 1, -1); ("check2", 1, 0); ("check2", 1, 1); ("rewind", -1, -1);
 ("rewind", -1, 0); ("rewind", -1, 1); ("rewind", 0, -1); ("rewind", 0, 0);
 ("rewind", 0, 1); ("rewind", 1, -1); ("rewind", 1, 0); ("rewind", 1, 1);
 ("go-end-1", -1, -1); ("go-end-1", -1, 0); ("go-end-1", -1, 1);
 ("go-end-1", 0, -1); ("go-end-1", 0, 0); ("go-end-1", 0, 1);
 ("go-end-1", 1, -1); ("go-end-1", 1, 0); ("go-end-1", 1, 1);
 ("go-end-2", -1, -1); ("go-end-2", -1, 0); ("go-end-2", -1, 1);
 ("go-end-2", 0, -1); ("go-end-2", 0, 0); ("go-end-2", 0, 1);
 ("go-end-2", 1, -1); ("go-end-2", 1, 0); ("go-end-2", 1, 1);
 ("skip", -1, -1); ("skip", -1, 0); ("skip", -1, 1); ("skip", 0, -1);
 ("skip", 0, 0); ("skip", 0, 1); ("skip", 1, -1); ("skip", 1, 0);
 ("skip", 1, 1); ("scan-1", -1, -1); ("scan-1", -1, 0); ("scan-1", -1, 1);
 ("scan-1", 0, -1); ("scan-1", 0, 0); ("scan-1", 0, 1); ("scan-1", 1, -1);
 ("scan-1", 1, 0); ("scan-1", 1, 1); ("scan-2", -1, -1); ("scan-2", -1, 0);
 ("scan-2", -1, 1); ("scan-2", 0, -1); ("scan-2", 0, 0); ("scan-2", 0, 1);
 ("scan-2", 1, -1); ("scan-2", 1, 0); ("scan-2", 1, 1);
 ("check-done", -1, -1); ("check-done", -1, 0); ("check-done", -1, 1);
 ("check-done", 0, -1); ("check-done", 0, 0); ("check-done", 0, 1);
 ("check-done", 1, -1); ("check-done", 1, 0); ("check-done", 1, 1)];
    input_alphabet = ["0";"1";"#"];
    tape_alphabet = ["0";"1";"#";"X";"_";">"];
    blank = "_";
    left_marker = ">";
    start = ("start",-1,-1);
    accept = ("acc",-1,-1);
    reject = ("rej",-1,-1);
    delta = (fun x -> match x with
    | (("start",-1,-1),">") -> (("check1",-1,-1),">",1)
    | (("check1",-1,-1),"0") -> (("check1",-1,-1),"0",1)
    | (("check1",-1,-1),"1") -> (("check1",-1,-1),"1",1)
    | (("check1",-1,-1),"#") -> (("check2",-1,-1),"#",1)
    | (("check2",-1,-1),"0") -> (("check2",-1,-1),"0",1)
    | (("check2",-1,-1),"1") -> (("check2",-1,-1),"1",1)
    | (("check2",-1,-1),"_") -> (("rewind",-1,1),"_",0)   (* start with a carry of 1! *)

    | (("rewind",-1,carry),">") -> (("go-end-1",-1,carry),">",1)
    | (("rewind",-1,carry),"0") -> (("rewind",-1,carry),"0",0)
    | (("rewind",-1,carry),"1") -> (("rewind",-1,carry),"1",0)
    | (("rewind",-1,carry),"#") -> (("rewind",-1,carry),"#",0)
    | (("rewind",-1,carry),"X") -> (("rewind",-1,carry),"X",0)

    | (("go-end-1",-1,carry),"#") -> (("scan-1",-1,carry),"#",0)
    | (("go-end-1",-1,carry),sym) -> (("go-end-1",-1,carry),sym,1)

    | (("scan-1",-1,carry),"X") -> (("scan-1",-1,carry),"X",0)
    | (("scan-1",-1,carry),"0") -> (("skip",0,carry),"X",1)
    | (("scan-1",-1,carry),"1") -> (("skip",1,carry),"X",1)
    | (("scan-1",-1,0),">") -> (("check-done",-1,-1),">",1)  (* carry should be 0 to be done *)

    | (("skip",v,carry),"#") -> (("go-end-2",v,carry),"#",1)
    | (("skip",v,carry),"X") -> (("skip",v,carry),"X",1)

    | (("go-end-2",v,carry),"_") -> (("scan-2",v,carry),"_",0)
    | (("go-end-2",v,carry),sym) -> (("go-end-2",v,carry),sym,1)

    | (("scan-2",v,carry),"X") -> (("scan-2",v,carry),"X",0)
    | (("scan-2",v,carry),"0") when (v+carry) mod 2 = 0 -> (("rewind",-1,(v+carry) / 2),"X",0)
    | (("scan-2",v,carry),"1") when (v+carry) mod 2 = 1 -> (("rewind",-1,(v+carry) / 2),"X",0)

    | (("check-done",-1,-1),"_") -> (("acc",-1,-1),"_",1)
    | (("check-done",-1,-1),"X") -> (("check-done",-1,-1),"X",1)
    | (("check-done",-1,-1),"#") -> (("check-done",-1,-1),"#",1)

    | (_,sym) -> (("rej",-1,-1),sym,1))}





(* QUESTION 3 *)
let funcTwo = (fun (x,y) -> x^"|"^(y))
  let funcT = (fun (x,y,z) -> x^"|"^(y)^"|"^(string_of_int z))


let permutation_h = 
  { states = [("s","-1");
              ("getInp","-1");
              ("rewind","-1");
              ("finish","-1");
              ("accept","-1");
              ("reject","-1")] @
              (pairs ["goRight"; "scanR"] ["-1";"a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"]);
    input_alphabet = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"];
    tape_alphabet = [">";"_";"X";"#";"a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"s";"t";"u";"v";"w";"x";"y";"z"];
    start = ("s","-1");
    accept = ("accept","-1");
    reject = ("reject","-1");
    blank = "_";
    left_marker = ">";
    delta = (fun x -> match x with
    | (("s","-1"),">") -> (("getInp","-1"),">",1)
    | (("s",_),sym) -> (("reject","-1"),sym,1)

    | (("getInp","-1"),"#") -> (("finish","-1"),"#",1)
    | (("getInp","-1"),"_") -> (("reject","-1"),"_",0)
    | (("getInp","-1"),"X") -> (("getInp","-1"),"X",1)
    | (("getInp","-1"),sym) -> (("goRight",sym),"X",1)

    | (("goRight",read),"#") -> (("scanR",read),"#",1)
    | (("goRight",read),"_") -> (("reject","-1"),"_",0)
    | (("goRight",read),"X") -> (("goRight",read),"X",1)
    | (("goRight",read),sym) -> (("goRight",read),sym,1)

    | (("scanR",read),"X") -> (("scanR",read),"X",1)
    | (("scanR",read),"_") -> (("reject","-1"),"_",0)
    | (("scanR",read),"#") -> (("reject","-1"),"#",1)
    | (("scanR",read),sym) -> if (sym=read) then (("rewind", "-1"), "X", 0)
                                else (("scanR", read), sym, 1)

    | (("rewind","-1"),">") -> (("getInp","-1"),">",1)
    | (("rewind","-1"),"X") -> (("rewind","-1"),"X",0)
    | (("rewind","-1"),"#") -> (("rewind","-1"),"#",0)
    | (("rewind","-1"),sym) -> (("rewind","-1"),sym,0)

    | (("finish","-1"),"X") -> (("finish","-1"),"X",1)
    | (("finish","-1"),"#") -> (("reject","-1"),"#",1)
    | (("finish","-1"),"_") -> (("accept","-1"),"_",1)


    |((_,_),sym) -> (("reject","-1"),sym,1)





  ) }

let permutation = transform permutation_h funcTwo


let copies_H n = 
  if (n < 1) then failwith "n has to be greater than 0" else
  {
  states = [("s","-1",0);
              ("getInp","-1", 0);
              ("rewind","-1", 0);
              ("accept","-1", 0);
              ("reject","-1", 0 )] @
              (triples ["verify";"ndec"; "nextH"] ["0";"1";"-1"] (range (n)));
  input_alphabet = ["0";"1"; "#"];
  tape_alphabet = [">";"X";"_";"0";"1";"#"];
  start = ("s","-1",0);
  accept = ("accept","-1",0);
  reject = ("reject","-1",0);
  blank = "_";
  left_marker = ">";
  delta = (fun x -> match x with
    | (("s","-1", 0),">") -> (("getInp","-1",0),">",1)
    | (("s",_,_),sym) -> (("reject","-1",0),sym,1)

    | (("getInp","-1", 0),"#") -> (("verify","-1",0),"#",1)
    | (("getInp","-1", 0),"X") -> (("getInp","-1",0),"X",1)
    | (("getInp","-1", 0),"_") -> if (n=1) then (("accept", "-1", 0), "_",1) 
                                  else (("reject", "-1", 0), "_", 1)
    | (("getInp","-1", 0),sym) -> (("nextH",sym,1),"X",1)


    | (("nextH",read,count),">") -> (("reject","-1",0),">",1)
    | (("nextH",read,count),"#") -> (("ndec",read,count+1),"#",1)
    | (("nextH",read,count),"X") -> (("nextH",read,count),"X",1)
    | (("nextH",read,count),"_") -> if (count = n) then (("accept", "-1", 0), "_",0)
                                      else (("reject", "-1", 0), "_", 1)
    | (("nextH",read,count),sym) -> (("nextH",read,count),sym,1)

    | (("ndec",read,count),">") -> (("reject","-1",0),">",1)
    | (("ndec",read,count),"#") -> (("reject","-1",0),"#",1)
    | (("ndec",read,count),"_") -> (("reject","-1",0),"_",1)
    | (("ndec",read,count),"X") -> (("ndec",read,count),"X",1)
    | (("ndec",read,count),sym) -> if (read<>sym) then (("reject","-1",0),sym,1)
                                    else if(count<n) then (("ndec",read,count),"X",1)
                                            else (("rewind","-1",0),"X",0)

    | (("rewind","-1", 0),">") -> (("getInp","-1",0),">",1)
    | (("rewind","-1", 0),"X") -> (("rewind","-1",0),"X",0)
    | (("rewind","-1", 0),sym) -> (("rewind","-1",0),sym,0)

    | (("verify","-1", count),"_") -> if(count = n) then (("accept", "-1",0),"_",0) else (("reject", "-1", 0), "_", 1)
    | (("verify","-1", count),"X") -> (("verify","-1",count),"X",1)
    | (("verify","-1", count),"#") -> (("verify","-1",count),"#",1)
    | (("verify","-1", count),sym) -> (("reject","-1",0),sym,1)
     | ((_,_,_),readChar) -> (("reject","-",-1),readChar,1)


  )
  }


  let copies n = transform (copies_H n) funcT
