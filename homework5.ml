(* 

HOMEWORK 5

Name: Nitya	Dhanushkodi	

Email: nitya.dhanushkodi@students.olin.edu

Remarks, if any: Got help reasoning through the turing machines. 

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
 * Helper function
 *
 * Pint a configuration (including newline) to standard output
 * and RETURN A VALUE
 * 
 *)

let printConfig m config value = 
    let mw = List.fold_right (fun a r -> max (String.length a) r) m.states 0 in
    let _ = print_string (String.sub (config.state^(String.make mw ' ')) 0 mw) in
    let print_syms = List.iter (Printf.printf " %s ")  in
    let _ = print_string "  "  in
    let _ = print_syms config.before  in
    let _ = (match config.after with 
             | [] -> Printf.printf "[%s]" m.blank
	     | a::v' -> let _ = Printf.printf "[%s]" a  in
	                print_syms v') in
    let _ = print_newline ()  in
    value




(* QUESTION 1 *)

(*

Code a function startConfig with type 
'a tm -> 'a config where startConfig m w yields 
the starting configuration for Turing machine m with w as 
input string. (Note that OCaml may report the type 
symbol as string — that's okay, since they are equivalent.
 So don't worry about that.)

# startConfig asbs "";;
- : string config = {state = "start"; before = []; after = [">"]}
# startConfig asbs "ab";;
- : string config = {state = "start"; before = []; after = [">"; "a"; "b"]}
# startConfig asbs "aaaabbbaa";;
- : string config =
{state = "start"; before = [];
 after = [">"; "a"; "a"; "a"; "a"; "b"; "b"; "b"; "a"; "a"]}
# startConfig anbn "";;
- : string config = {state = "start"; before = []; after = ["|"]}
# startConfig anbn "aabb";;
- : string config =
{state = "start"; before = []; after = ["|"; "a"; "a"; "b"; "b"]}
# startConfig anbn "aabbaa";;
- : string config =
{state = "start"; before = []; after = ["|"; "a"; "a"; "b"; "b"; "a"; "a"]}

*)
let startConfig m w = 
	{state = m.start; before = []; after= m.left_marker::explode(w)}

(*Code functions acceptConfig, rejectConfig, and haltConfig, 
each of type 'a tm -> 'a config -> bool where 
acceptConfig m c (resp., rejectConfig m c, haltConfig m c) 
returns true if and only if c is an accepting 
(resp., rejecting, halting) configuration for Turing machine m.

# acceptConfig asbs {state="start"; before=[]; after=["_"]};;
- : bool = false
# acceptConfig asbs {state="q1"; before=[]; after=[">";"a";"_"]};;
- : bool = false
# acceptConfig asbs {state="acc"; before=["b"]; after=[">";"a";"_"]};;
- : bool = true
# acceptConfig asbs {state="rej"; before=["b"]; after=[">";"a";"_"]};;
- : bool = false
# rejectConfig asbs {state="start"; before=[]; after=["_"]};;
- : bool = false
# rejectConfig asbs {state="q1"; before=[]; after=[">";"a";"_"]};;
- : bool = false
# rejectConfig asbs {state="acc"; before=["b"]; after=[">";"a";"_"]};;
- : bool = false
# rejectConfig asbs {state="rej"; before=["b"]; after=[">";"a";"_"]};;
- : bool = true
# haltConfig asbs {state="start"; before=[]; after=["_"]};;
- : bool = false
# haltConfig asbs {state="q1"; before=[]; after=[">";"a";"_"]};;
- : bool = false
# haltConfig asbs {state="acc"; before=["b"]; after=[">";"a";"_"]};;
- : bool = true
# haltConfig asbs {state="rej"; before=["b"]; after=[">";"a";"_"]};;
- : bool = true

*)
let acceptConfig m config = 
	m.accept = config.state


let rejectConfig m config = 
	m.reject = config.state


let haltConfig m c = 
	c.state = m.accept || c.state = m.reject

(*Code a function step with type 
'a tm -> 'a config -> 'a config where 
step m c returns the configuration obtained by 
taking one step of Turing machine m from configuration c. 
Function step m is basically the relation C →1 D described in lecture.

# step asbs {state="start"; before=[]; after=["_"]};;
- : string config = {state = "acc"; before = ["_"]; after = []}
# step asbs {state="start"; before=[">";"a"]; after=["b";"b"]};;
- : string config = {state = "q1"; before = [">"; "a"; "b"]; after = ["b"]}
# step asbs {state="q1"; before=[">";"a"]; after=["a";"b"]};;
- : string config = {state = "rej"; before = [">"; "a"; "a"]; after = ["b"]}
# step asbs {state="q1"; before=[">";"a"]; after=["b";"b"]};;
- : string config = {state = "q1"; before = [">"; "a"; "b"]; after = ["b"]}
# step anbn {state="q1"; before=["|";"a";"b"]; after=["/"]};;
- : string config = {state = "q2"; before = ["|"; "a"; "b"; "/"]; after = []}
# step anbn {state="q2"; before=["|";"a";"b"]; after=["/"]};;
- : string config = {state = "q2"; before = ["|"; "a"]; after = ["b"; "/"]}
# step anbn {state="q3"; before=["|"]; after=["a";"b"]};;
- : string config = {state = "q4"; before = ["|"; "X"]; after = ["b"]}
# step anbn {state="q4"; before=["|";"X"]; after=["b"]};;
- : string config = {state = "q2"; before = ["|"; "X"; "X"]; after = []}

*)
let fst xs = 
	match xs with first::rest -> first

let rest xs = 
	match xs with first::rest -> rest


let last xs = 
	fst (List.fold_right (fun x acc -> acc@[x]) xs [])

let rec allButLast xs = 
	match xs with
	first::[] -> []
	|first::rest -> first::allButLast(rest)
	

(*step anbn {state="q2"; before=["|";"a";"b"]; after=["/"]};;
my output is different but idk why this  ones output is 
config = {state = "q2"; before = ["|"; "a"]; after = ["b"; "/"]} without 1 more / in the after*)
let step m config = 
	match m.delta (config.state, (fst config.after) ) with 
	(q,w,d) -> if d=1 then {state=q; before= config.before@[w]; after= if (rest config.after) = [] then [m.blank] else (rest config.after)}
					else {state =q; before= (allButLast config.before ); after= (last config.before)::w::(rest config.after)}				


(* Code a function run with type string tm -> string -> bool 
where run m w returns true if m (which should be a Turing machine 
where states are described using strings) accepts input string w, 
and returns false if m rejects input string w. The function should,
 as a side effect, also print the sequence of configurations that 
 Turing machine m goes through during its computation.

I provided you with a function printConfig to print a configuration, 
which takes the configuration to print and also a value for printConfig 
to return. Thus, printConfig c v will print configuration c and return value v.

# run asbs "aab";;
start  [>] a  a  b 
start   > [a] a  b 
start   >  a [a] b 
start   >  a  a [b]
q1      >  a  a  b [_]
acc     >  a  a  b  _ [_]
- : bool = true*)
let rec run_helper m w config = 

	if haltConfig m (printConfig m config config) then (acceptConfig m config) else run_helper m w (step m config)
let run m w = 
	run_helper m w (startConfig m w)
	
	




(* 
 * Some sample deterministic Turing machines
 *
 * asbs is the regular language {a^m b^n | m,n >= 0}
 * anbn is the non-regular language {a^n b^n | n >= 0}
 * anbncn is the non-regular language {a^n b^n c^n | n >= 0}
 *
 *)

let asbs = { states = ["start"; "q1"; "acc"; "rej"];
	     input_alphabet = ["a";"b"];
	     tape_alphabet = ["a";"b";"_";">"];
	     blank = "_";
	     left_marker = ">";
	     start = "start";
	     accept = "acc";
	     reject = "rej";
	     delta = (fun inp -> match inp with
	                 | ("start", "a") -> ("start", "a", 1)
     			 | ("start", "b") -> ("q1", "b", 1)
			 | ("start", ">") -> ("start", ">", 1)
			 | ("start", "_") -> ("acc", "_", 1)
			 | ("q1", "b") -> ("q1", "b", 1)
			 | ("q1", "_") -> ("acc", "_", 1)
			 | ("acc", "a") -> ("acc", "a", 1)
			 | ("acc", "b") -> ("acc", "b", 1)
			 | ("acc", ">") -> ("acc", ">", 1)
			 | ("acc", "_") -> ("acc", "_", 1)
			 | (_,c) -> ("rej",c,1))}

let anbn = { states = ["start"; "q1"; "q2"; "q3"; "q4"; "acc"; "rej"];
	     input_alphabet = ["a";"b"];
	     tape_alphabet = ["a";"b";"X";"/";"|"];
	     blank = "/";
	     left_marker = "|";
	     start = "start";
	     accept = "acc";
	     reject = "rej";
	     delta = (fun inp -> match inp with
	                 | ("start", "a") -> ("start", "a", 1)
     			 | ("start", "b") -> ("q1", "b", 1)
			 | ("start", "|") -> ("start", "|", 1)
			 | ("start", "/") -> ("q2", "/", 1)
			 | ("q1", "b") -> ("q1", "b", 1)
			 | ("q1", "/") -> ("q2", "/", 1)
			 | ("q2", "|") -> ("q3", "|", 1)
			 | ("q2", "a") -> ("q2", "a", 0)
			 | ("q2", "b") -> ("q2", "b", 0)
			 | ("q2", "X") -> ("q2", "X", 0)
			 | ("q2", "/") -> ("q2", "/", 0)
			 | ("q3", "X") -> ("q3", "X", 1)
			 | ("q3", "/") -> ("acc", "/", 1)
			 | ("q3", "a") -> ("q4", "X", 1)
			 | ("q4", "a") -> ("q4", "a", 1)
			 | ("q4", "X") -> ("q4", "X", 1)
			 | ("q4", "b") -> ("q2", "X", 1)
			 | ("acc", "a") -> ("acc", "a", 1)
			 | ("acc", "b") -> ("acc", "b", 1)
			 | ("acc", "|") -> ("acc", "|", 1)
			 | ("acc", "X") -> ("acc", "X", 1)
			 | ("acc", "/") -> ("acc", "/", 1)
			 | (_,c) -> ("rej",c,1))}


let anbncn = { states = ["start";"q1";"q2";"q3";"q4";"q5";"q6";"acc";"rej"];
	       input_alphabet = ["a";"b";"c"];
	       tape_alphabet = ["a";"b";"c";"X";"_";">"];
	       blank = "_";
	       left_marker = ">";
	       start = "start";
	       accept = "acc";
	       reject = "rej";
	       delta = (fun inp -> match inp with
	                | ("start", "a") -> ("start", "a", 1)
     			| ("start", "b") -> ("q1", "b", 1)
			| ("start", "c") -> ("q6", "c", 1)
			| ("start", ">") -> ("start", ">", 1)
			| ("start", "_") -> ("q2", "_", 1)
			| ("q1", "b") -> ("q1", "b", 1)
			| ("q1", "c") -> ("q6", "c", 1)
			| ("q1", "_") -> ("q2", "_", 1)
			| ("q2", ">") -> ("q3", ">", 1)
			| ("q2", "a") -> ("q2", "a", 0)
			| ("q2", "b") -> ("q2", "b", 0)
			| ("q2", "c") -> ("q2", "c", 0)
			| ("q2", "_") -> ("q2", "_", 0)
			| ("q2", "X") -> ("q2", "X", 0)
			| ("q3", "X") -> ("q3", "X", 1)
			| ("q3", "_") -> ("acc", "_", 1)
			| ("q3", "a") -> ("q4", "X", 1)
			| ("q4", "a") -> ("q4", "a", 1)
			| ("q4", "X") -> ("q4", "X", 1)
			| ("q4", "b") -> ("q5", "X", 1)
			| ("q5", "b") -> ("q5", "b", 1)
			| ("q5", "X") -> ("q5", "X", 1)
			| ("q5", "c") -> ("q2", "X", 1)
			| ("q6", "c") -> ("q6", "c", 1)
			| ("q6", "_") -> ("q2", "_", 1)
		        | ("acc", "a") -> ("acc", "a", 1)
		        | ("acc", "b") -> ("acc", "b", 1)
		        | ("acc", "c") -> ("acc", "c", 1)
		        | ("acc", ">") -> ("acc", ">", 1)
		        | ("acc", "X") -> ("acc", "X", 1)
		        | ("acc", "_") -> ("acc", "_", 1)
			| (_,c) -> ("rej", c,1))}

(* QUESTION 2 *)

(* THESE ARE PLACEHOLDERS - THEY DEFINE EMPTY TURING MACHINES *)
(* REPLACE BY YOUR OWN DEFINITIONS *)


let tm_q2_a = { states = ["s";"q1";"q2";"q3";"q4";"q5";"q6";"q7";"ac";"re"];
		input_alphabet = ["c";"d"];
		tape_alphabet = ["c";"d";"X";"_";">"];
		blank = "_";
		left_marker = ">";
		start = "s";
		accept = "ac";
		reject = "re";
		delta = (fun inp -> match inp with
	        | ("s", ">") -> ("q1", ">", 1)
	        | ("q1", "_") -> ("ac", "_", 1)
			| ("q1", "c") -> ("q2", "X", 1)
			| ("q1", "d") -> ("q5", "X", 1)
			| ("q1", "X") -> ("q7", "X", 1)
			| ("q2", "c") -> ("q2", "c", 1)
			| ("q2", "d") -> ("q2", "d", 1)
			| ("q2", "X") -> ("q3", "X", 0)
			| ("q2", "_") -> ("q3", "_", 0)
			| ("q3", "c") -> ("q4", "X", 0)
			| ("q3", "d") -> ("re", "X", 1)
			| ("q3", "X") -> ("ac", "X", 1)
			| ("q4", "c") -> ("q4", "c", 0)
			| ("q4", "d") -> ("q4", "d", 0)
			| ("q4", "X") -> ("q4", "X", 0)
			| ("q4", ">") -> ("q1", ">", 1)
			| ("q5", "c") -> ("q5", "c", 1)
			| ("q5", "d") -> ("q5", "d", 1)
			| ("q5", "X") -> ("q6", "X", 0)
			| ("q5", "_") -> ("q6", "_", 0)
			| ("q6", "c") -> ("re", "X", 1)
			| ("q6", "d") -> ("q4", "X", 0)
			| ("q6", "X") -> ("ac", "X", 1)
		    | ("q7", "c") -> ("q2", "X", 1)
	        | ("q7", "d") -> ("q5", "X", 1)
	        | ("q7", "X") -> ("q7", "X", 1)
	        | ("q7", "_") -> ("ac", "_", 0)
	        | ("ac", "_") -> ("ac", "_", 0)
			| (_,c) -> ("re", c,1))}


let tm_q2_b = { states = ["s";"q1";"q2";"q3";"q4";"q5";"q6";"q7";"ac";"re"];
		input_alphabet = ["a";"b"];
		tape_alphabet = ["a";"b";"X";"_";">"];
		blank = "_";
		left_marker = ">";
		start = "s";
		accept = "ac";
		reject = "re";
		delta = (fun inp -> match inp with
	        | ("s", ">") -> ("q1", ">", 1)
	        | ("q1", "_") -> ("ac", "_", 1)
			| ("q1", "a") -> ("re", "X", 1)
			| ("q1", "b") -> ("q3", "X", 1)
			| ("q1", "X") -> ("q2", "X", 1)
			| ("q2", "a") -> ("re", "X", 1)
			| ("q2", "b") -> ("q3", "X", 1)
			| ("q2", "X") -> ("q2", "X", 1)
			| ("q2", "_") -> ("ac", "_", 0)
			| ("q3", "a") -> ("q3", "a", 1)
			| ("q3", "b") -> ("q3", "b", 1)
			| ("q3", "X") -> ("q3", "X", 1)
			| ("q3", "_") -> ("q4", "_", 0)
			| ("q4", "X") -> ("q4", "X", 0)
			| ("q4", "a") -> ("q5", "X", 0)
			| ("q5", "a") -> ("q6", "X", 0)
			| ("q6", "a") -> ("q7", "X", 0)

		    | ("q7", "a") -> ("q7", "a", 0)
	        | ("q7", "b") -> ("q7", "b", 0)
	        | ("q7", "X") -> ("q7", "X", 0)
	        | ("q7", ">") -> ("q1", ">", 1)

	        | ("ac", "_") -> ("ac", "_", 0)
	        | ("ac", "X") -> ("ac", "X", 0)
			| (_,c) -> ("re", c,1))}




(* QUESTION 3 *)


let binaryAddition = { states = ["s"; "q1";"q2";"q3";"q4";"q5";"q6";"q7";
"q8";"q9";"q10";"q11";"q12";"q13";"q14";"q15";"q16";"q17";"q18";"q19";"q20";"q21"; 
"a0r1n"; "a1r0n"; "a0r1y"; "a1r1y"; "rewNC"; "rewYC";"ac"; "re"];
		       input_alphabet = ["0"; "1"; "#"];
		       tape_alphabet = ["0"; "1"; "#"; "X"; "_"; ">"];
		       blank = "_";
		       left_marker = ">";
		       start = "s";
		       accept = "ac";
		       reject = "re";
		       delta = (fun inp -> match inp with
	        | ("s", ">") -> ("q1", ">", 1)
	        | ("q1", "0") -> ("q1", "0", 1)
			| ("q1", "1") -> ("q1", "1", 1)
			| ("q1", "#") -> ("q2", "#", 0)
			| ("q1", "X") -> ("q2", "X", 0)

			| ("q2", "0") -> ("q3", "X", 1)
			| ("q2", ">") -> ("q21", ">", 1)
			| ("q2", "1") -> ("q8", "X", 1)


			| ("q3", "#") -> ("q4", "#", 1)
			| ("q3", "X") -> ("q3", "X", 1)

			| ("q4", "0") -> ("q4", "0", 1)
			| ("q4", "1") -> ("q4", "1", 1)
			| ("q4", "X") -> ("q5", "X", 0)
			| ("q4", "#") -> ("q5", "#", 0)


			| ("q5", "0") -> ("a0r1n", "X", 1)
			| ("q5", "1") -> ("a1r0n", "X", 1)

			| ("a0r1n", "0") -> ("a0r1n", "0", 1)
			| ("a0r1n", "1") -> ("a0r1n", "1", 1)
			| ("a0r1n", "#") -> ("a0r1n", "#", 1)
			| ("a0r1n", "X") -> ("a0r1n", "X", 1)
			| ("a0r1n", "_") -> ("q6", "_", 0)

			| ("q6", "0") -> ("rewNC", "X", 0)
			| ("q6", "X") -> ("q6", "X", 0)

			| ("rewNC", "0") -> ("rewNC", "0", 0)
			| ("rewNC", "1") -> ("rewNC", "1", 0)
			| ("rewNC", "#") -> ("rewNC", "#", 0)
			| ("rewNC", "X") -> ("rewNC", "X", 0)
			| ("rewNC", ">") -> ("q1", ">", 1)

			| ("a1r0n", "0") -> ("a1r0n", "0", 1)
			| ("a1r0n", "1") -> ("a1r0n", "1", 1)
			| ("a1r0n", "#") -> ("a1r0n", "#", 1)
			| ("a1r0n", "X") -> ("a1r0n", "X", 1)
			| ("a1r0n", "_") -> ("q7", "_", 0)			

		    | ("q7", "1") -> ("rewNC", "X", 0)
		    | ("q7", "X") -> ("q7", "X", 0)

		    | ("q21", "X") -> ("q22", "X", 1)

		    | ("q22", "X") -> ("q22", "X", 1)
		    | ("q22", "#") -> ("q22", "#", 1)
			| ("q22", "_") -> ("ac", "_", 1)

			| ("q8", "#") -> ("q9", "#", 1)
			| ("q8", "X") -> ("q8", "X", 1)

			| ("q9", "0") -> ("q9", "0", 1)
			| ("q9", "1") -> ("q9", "1", 1)
			| ("q9", "X") -> ("q10", "X", 0)
			| ("q9", "#") -> ("q10", "#", 0)

			| ("q10", "0") -> ("a1r0n", "X", 1)
			| ("q10", "1") -> ("a0r1y", "X", 1)

			| ("a0r1y", "0") -> ("a0r1y", "0", 1)
			| ("a0r1y", "1") -> ("a0r1y", "1", 1)
			| ("a0r1y", "X") -> ("a0r1y", "X", 1)
			| ("a0r1y", "#") -> ("a0r1y", "#", 1)
			| ("a0r1y", "_") -> ("q11", "_", 0)

			| ("q11", "0") -> ("rewYC", "X", 0)
			| ("q11", "X") -> ("q11", "X", 0)

			| ("rewYC", "0") -> ("rewYC", "0", 0)
			| ("rewYC", "1") -> ("rewYC", "1", 0)
			| ("rewYC", "X") -> ("rewYC", "X", 0)
			| ("rewYC", "#") -> ("rewYC", "#", 0)
			| ("rewYC", ">") -> ("q12", ">", 1)

			| ("q12", "0") -> ("q12", "0", 1)
			| ("q12", "1") -> ("q12", "1", 1)
			| ("q12", "#") -> ("q13", "#", 0)
			| ("q12", "X") -> ("q13", "X", 0)

			| ("q13", "0") -> ("q14", "X", 1)
			| ("q13", "1") -> ("q17", "X", 1)
			| ("q13", ">") -> ("q21", ">", 1)

			| ("q14", "#") -> ("q15", "#", 1)
			| ("q14", "X") -> ("q14", "X", 1)

			| ("q15", "0") -> ("q15", "0", 1)
			| ("q15", "1") -> ("q15", "1", 1)
			| ("q15", "X") -> ("q16", "X", 0)
			| ("q15", "#") -> ("q16", "#", 0)

			| ("q16", "0") -> ("a1r0n", "X", 1)
			| ("q16", "1") -> ("a0r1y", "X", 1)

			| ("q17", "#") -> ("q18", "#", 1)
			| ("q17", "X") -> ("q17", "X", 1)

			| ("q18", "0") -> ("q18", "0", 1)
			| ("q18", "1") -> ("q18", "1", 1)
			| ("q18", "X") -> ("q19", "X", 0)
			| ("q18", "#") -> ("q19", "#", 0)

			| ("q19", "0") -> ("a0r1y", "X", 1)
			| ("q19", "1") -> ("a1r1y", "X", 1)

			| ("a1r1y", "0") -> ("a1r1y", "0", 1)
			| ("a1r1y", "1") -> ("a1r1y", "1", 1)
			| ("a1r1y", "X") -> ("a1r1y", "X", 1)
			| ("a1r1y", "#") -> ("a1r1y", "#", 1)
			| ("a1r1y", "_") -> ("q20", "_", 0)

			| ("q20", "1") -> ("rewYC", "X", 0)
			| ("q20", "X") -> ("q20", "X", 0)



	        | ("ac", "_") -> ("ac", "_", 1)

			| (_,c) -> ("re", c,1))}

