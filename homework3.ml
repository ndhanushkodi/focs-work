(* 

HOMEWORK 3

Name: Nitya Dhanushkodi 

Email: nitya.dhanushkodi@students.olin.edu

Remarks, if any: Sarah and Cynthia helped me debug for the designing NFA's part!

*)


(*
 *
 * Please fill in this file with your solutions and submit it
 *
 * The functions below are stubs that you should replace with your
 * own implementation.
 *
 * Always make sure you can #use this file before submitting it.
 * It has to load without any errors.
 *
 *)



(* 
 * String <-> characters utility functions:
 *
 *   explode : string -> char list
 *      returns the list of characters making up a string
 *
 *   implode : char list -> string
 *      concatenates the list of characters into a string
 *
 *)

let explode (str) = 
  let rec acc (index,result) = 
    if (index<0) then
      result
    else
      acc(index-1, (String.get str index)::result)
  in
    acc(String.length(str)-1, [])

let implode (cs) = 
  let str = String.create(List.length(cs)) in
  let rec loop (cs,index) = 
    match cs with
      [] -> str
    | c::cs -> (String.set str index c; loop(cs,index+1))
  in
    loop(cs,0)



(*
 *  The type of a finite automaton
 * 
 *  When the transition relation is a function
 *  (i.e., every p,a has q such that (p,a,q) is in 
 *  delta) then this is a deterministic finite automaton  
 * 
 *)

type 'a fa = { states: 'a list;
               alphabet: char list;
               delta: ('a * char * 'a) list;
               start : 'a;
               accepting : 'a list }


let dfaThreeA = { 
     states = ["start";"one";"two"];
     alphabet = ['a';'b'];
     delta = [ ("start",'a',"one");
               ("one",'a',"two");
               ("two",'a',"start");
               ("start",'b',"start");
               ("one",'b',"one");
               ("two",'b',"two") ];
     start = "start";
     accepting = ["start"]
   } 

let nfaLastThreeB = {
     states = [0;1;2;3];
     alphabet = ['a';'b';'c'];
     delta = [ (0,'a',0);
               (0,'b',0);
               (0,'c',0);
               (0,'b',1);
               (1,'b',2);
               (2,'b',3); ];
     start = 0;
     accepting = [3]
   } 

(* QUESTION 1 *)

let rec findTransitions_helper (delta,q,a) = 
  match delta with [] -> []
    | (x,y,z)::rest -> if x=q & y=a then (x,y,z)::findTransitions_helper(rest,q,a) else findTransitions_helper(rest,q,a)

let findTransitions (fa,q,a) = 
  findTransitions_helper(fa.delta, q, a)





let rec isAccepting_helper (fa_accept,s) = 
  match fa_accept with [] -> false
    |first::rest -> if first = s then true else isAccepting_helper(rest, s)

let isAccepting (fa,s) = 
  isAccepting_helper(fa.accepting,s)





let step (fa,q,a) = 
  match findTransitions(fa,q,a) with [] -> failwith "no step"
    |(x,y,z)::[] -> z




let rec steps (fa,q,syms) = 
  match syms with [] -> q
    |first::rest -> steps(fa, step(fa,q,first), rest)





(*Code a function isDFA of type 
'a fa -> bool which takes a finite automaton
 m and returns true if m is deterministic, 
 and false otherwise. Basically, it checks whether 
 every state and every symbol has exactly one transition 
 that applies.

# isDFA (dfaThreeA);;
- : bool = true
# isDFA (nfaLastThreeB);;
- : bool = false
# isDFA {states=[0;1]; 
         alphabet=['a']; 
         delta=[(0,'a',1)]; 
         start=0; 
         accepting=[1]};;
- : bool = false

*)

let rec isDFA_helper (fa, states, alphabet, alpha_save) = 
  match states with [] -> true
  |first_s::rest_s -> 
    match alphabet with [] -> isDFA_helper(fa, rest_s, alpha_save, alpha_save)
    |first_a::rest_a -> if List.length(findTransitions(fa, first_s, first_a)) !=1 then false else isDFA_helper(fa, [first_s]@rest_s, rest_a, alpha_save)
                    

let isDFA (fa) = 
  isDFA_helper(fa, fa.states, fa.alphabet, fa.alphabet)


let rec setIn (e,xs) = 
   match xs with [] -> false
      |first::rest -> if e=first then true else setIn(e,rest)

let acceptDFA (fa,input) = 
  if isDFA(fa) then  setIn(steps(fa, fa.start, explode(input)) , fa.accepting)  else failwith "not a dfa"






(* QUESTION 2 *)

(* THESE ARE PLACEHOLDERS - THEY DEFINE EMPTY AUTOMATA *)
(* REPLACE BY YOUR OWN DEFINITIONS *)


let dfa_q2_a = { states = [0;1;2;3];
		 alphabet = ['a';'b'];
		 delta = [
     (0, 'a', 0);
     (0, 'b', 1);
     (1, 'a', 0);
     (1, 'b', 2);
     (2, 'a', 0);
     (2, 'b', 3);
     (3, 'a', 3);
     (3, 'b', 3);
      ];
		 start = 0;
		 accepting = [0;1;2]}


let dfa_q2_b = { states = [0;1;2;3;4;5;6;7;8];
		 alphabet = ['a';'b'];
		 delta = [
     (0, 'a', 1);
     (0, 'b', 5);
     (1, 'a', 2);
     (1, 'b', 5);
     (2, 'a', 3);
     (2, 'b', 5);
     (3, 'a', 4);
     (3, 'b', 5);
     (4, 'a', 4);
     (4, 'b', 4);
     (5, 'a', 1);
     (5, 'b', 6);
     (6, 'a', 1);
     (6, 'b', 7);
     (7, 'a', 1);
     (7, 'b', 8);
     (8, 'a', 8);
     (8, 'b', 8);
      ];
		 start = 0;
		 accepting = [0;1;2;3;5;6;7]}


let dfa_q2_c = { states = [0;1;2;3;4;5;6;7];
		 alphabet = ['a';'b'];
		 delta = [ 
     (0, 'a', 1);
     (0, 'b', 4);
     (1, 'a', 2);
     (1, 'b', 7);
     (2, 'a', 3);
     (2, 'b', 7);
     (3, 'a', 3);
     (3, 'b', 4);
     (4, 'a', 7);
     (4, 'b', 5);
     (5, 'a', 7);
     (5, 'b', 6);
     (6, 'a', 1);
     (6, 'b', 6);
     (7, 'a', 7);
     (7, 'b', 7);

     ];
		 start = 0;
		 accepting = [3;6]}


let nfa_q2_d = { states = [0;1;2;3;4;5;6];
		 alphabet = ['a';'b'];
		 delta = [ 
     (0, 'a', 1);
     (0, 'a', 5);
     (0, 'b', 4);
     (1, 'a', 2);
     (1, 'b', 4);
     (2, 'a', 3);
     (2, 'b', 4);
     (3, 'a', 3);
     (3, 'b', 6);
     (4, 'a', 4);
     (4, 'b', 4);
     (5, 'a', 5);
     (6, 'b', 6);
     (6, 'a', 5);
     (6, 'a', 6)
     ];
		 start = 0;
		 accepting = [4;5]}




(* QUESTION 3 *)

(*
Code a function keepTarget of type
 ('a * 'b * 'c) list -> 'c list which takes 
 a list of transitions (of the kind found in the 
 delta field of an automaton) and returns the list 
 of all states that those transitions lead to, the 
 targets. (See examples below.) For your own sanity, please 
 make sure you remove duplicates from the resulting list.

# keepTarget [];;
- : 'a list = []
# keepTarget [(1,'a',2);(1,'b',3)];;
- : int list = [2; 3]
# keepTarget [(1,'a',2);(1,'b',3);(2,'a',2)];;
- : int list = [3; 2]
# keepTarget (dfaThreeA.delta);;
- : string list = ["start"; "one"; "two"]
# keepTarget (nfaLastThreeB.delta);;
- : int list = [0; 1; 2; 3]

*)
let rec remove_dups lst= match lst with 
  | [] -> []
  | h::t -> h::(remove_dups (List.filter (fun x -> x<>h) t))

let rec keepTarget_h (trs) = 
  match trs with [] -> []
  |(x,y,z)::rest -> z::keepTarget_h(rest)

let rec keepTarget (trs) = 
  remove_dups(keepTarget_h(trs))



(*
Code a function isAcceptingAny of 
type 'a fa * 'a list -> bool which takes
 a finite automaton m and a list of states 
 qs of m and returns true when any of the states 
 in qs is an accepting state and false otherwise.

# isAcceptingAny (nfaLastThreeB, []);;
- : bool = false
# isAcceptingAny (nfaLastThreeB, [0]);;
- : bool = false
# isAcceptingAny (nfaLastThreeB, [0;1]);;
- : bool = false
# isAcceptingAny (nfaLastThreeB, [0;1;2]);;
- : bool = false
# isAcceptingAny (nfaLastThreeB, [0;1;2;3]);;
- : bool = true
# isAcceptingAny (nfaLastThreeB, [3]);;
- : bool = true

*)
let rec isAcceptingAny (fa,qs) = 
  match qs with [] -> false
  |first::rest -> if setIn(first, fa.accepting) then true else isAcceptingAny(fa, rest)


(*Code a function stepAll of type 
'a fa * 'a list * char -> 'a list which 
takes a finite automaton m, a list of 
statesqs of m, and a symbol a of the alphabet of m, 
and returns the list of all states of m resulting 
from taking a transition from any state in qs following 
symbol a. Again, for your sanity, you will probably want to make 
sure there are no repetitions in the resulting list.

# stepAll (dfaThreeA,[],'a');;
- : string list = []
# stepAll (dfaThreeA,["start"],'a');;
- : string list = ["one"]
# stepAll (dfaThreeA,["start"],'b');;
- : string list = ["start"]
# stepAll (dfaThreeA,["start";"one"],'a');;
- : string list = ["one"; "two"]
# stepAll (dfaThreeA,["start";"one"],'b');;
- : string list = ["start"; "one"]
# stepAll (nfaLastThreeB,[0;1],'a');;
- : int list = [0]
# stepAll (nfaLastThreeB,[0;1],'b');;
- : int list = [0; 1; 2]

*)

(* let rec stepAll_nfa_helper(trs,q,a) = 
  match trs with [] -> []
    |(x,y,z)::rest -> z::stepAll_nfa_helper(rest, q, a)

let trsToState(fa, q, a) =
  stepAll_nfa_helper(findTransitions(fa,q,a), q, a)
  

let stepAll (fa,qs,a) = 
  match qs with [] -> [] 
  |first::rest -> (stepAll_nfa_helper(first))@stepAll_nfa_helper(rest) *)

let rec stepAll_helper(fa,qs,a) = 
  match qs with [] -> []
  |first::rest -> keepTarget(findTransitions(fa,first,a))@stepAll_helper(fa, rest, a)

let rec stepAll(fa,qs,a) = 
  remove_dups(stepAll_helper(fa,qs,a))


(*

Code a function stepsAll of 
type 'a fa * 'a list * char list -> 'a list
 which takes a finite automaton m, a list of states 
 qs of m, and a list of symbols l of the alphabet of m, and
 returns the list of all states of m resulting from following 
 the transitions of m from any state in qs according to the symbols in l.

# stepsAll (dfaThreeA,[],[]);;
- : string list = []
# stepsAll (dfaThreeA,[],['a']);;
- : string list = []
# stepsAll (dfaThreeA,[],['a';'b']);;
- : string list = []
# stepsAll (dfaThreeA,["start"],[]);;
- : string list = ["start"]
# stepsAll (dfaThreeA,["start"],['a']);;
- : string list = ["one"]
# stepsAll (dfaThreeA,["start"],['a';'b']);;
- : string list = ["one"]
# stepsAll (dfaThreeA,["start"],['a';'a']);;
- : string list = ["two"]
# stepsAll (dfaThreeA,["start";"one"],['a';'a']);;
- : string list = ["two"; "start"]
# stepsAll (dfaThreeA,["start";"one"],['a';'a';'b']);;
- : string list = ["two"; "start"]
# stepsAll (dfaThreeA,["start";"one"],['a';'a';'b';'a']);;
- : string list = ["start"; "one"]
# stepsAll (nfaLastThreeB,[0;1],['a';'b';'b';'b']);;
- : int list = [0; 1; 2; 3]

*)
let rec stepsAll (fa,qs,syms) = 
  match syms with [] -> qs
  |first::[] -> stepAll(fa,qs,first)
  |first::rest -> stepsAll(fa, stepAll(fa,qs, first), rest)



(*Code a function acceptNFA of type 'a fa * string -> bool 
which takes a finite automaton m and a string s over the 
alphabet of m, and returns true if m accepts s, and false 
otherwise. Think of it as implementing the "multiple pebbles" 
interpretation of finite automata, following all paths in the 
automaton that apply at the same time..

You can use function explode (provided in the homework code) to turn a string into a list of characters.

For testing purposes, I've provided you also with a function langNFA where langNFA(m,k) prints out all the strings of length up to k accepted by the finite automaton m. Note that it calls your function acceptNFA, so it won't work correctly until you implement that.

# acceptNFA (dfaThreeA,"babab");;
- : bool = false
# acceptNFA (dfaThreeA,"bababa");;
- : bool = true
# acceptNFA (dfaThreeA,"bababab");;
- : bool = true
# acceptNFA (nfaLastThreeB,"abb");;
- : bool = false
# acceptNFA (nfaLastThreeB,"abbb");;
- : bool = true
# langNFA (nfaLastThreeB,7);;*)
let acceptNFA (fa,input) = 
  isAcceptingAny(fa, stepsAll(fa, [fa.start], explode(input)))




(* 
 * A sample DFA for testing
 *
 * It accepts the language of all strings over {a,b} with a
 * multiple-of-3 number of a's.
 *
 *)

let dfaThreeA = { 
  states = ["start";"one";"two"];
  alphabet = ['a';'b'];
  delta = [ ("start",'a',"one");
	    ("one",'a',"two");
	    ("two",'a',"start");
	    ("start",'b',"start");
	    ("one",'b',"one");
	    ("two",'b',"two") ];
  start = "start";
  accepting = ["start"]
} 



(* A sample NFA for testing
 *
 * It accepts the language of all strings over {a,b,c} 
 * whose last three symbols are b's.
 *
 *)

let nfaLastThreeB = {
  states = [0;1;2;3];
  alphabet = ['a';'b';'c'];
  delta = [ (0,'a',0);
	    (0,'b',0);
	    (0,'c',0);
	    (0,'b',1);
	    (1,'b',2);
	    (2,'b',3); ];
  start = 0;
  accepting = [3]
} 




(* This function is the base function that langDFA and
 * langNFA use -- it basically loops through all the strings
 * of length up to n, and prints those that are accepted by the
 * finite automaton.
 *
 * This is being way too clever to try to not blow the stack 
 * while enumerating all strings up to a given length. Basically.
 * we enumerate all integer, convert them to base K (where K is the
 * size of the alphabet) and then replace every digit base K by the
 * letter of the alphabet at the corresponding index in the alphabet. 
 *
 * The key is that we can enumerate integers super easily
 *
 *)

let langFA accept (fa,n) = 

  let rec expt a n = if n <= 0 then 1 else a*(expt a (n-1)) in
  
  let rec take n default l = 
    if n <= 0 then []
    else (match l with
          | [] -> default::(take (n-1) default l)
          | x::xs -> x::(take (n-1) default xs)) in
  
  let to_base_n base size n = 
    let rec loop n = 
      if n <= 0 then []
      else if n mod base = 0 then 0::(loop (n / base))
      else (n mod base)::(loop ((n - n mod base) / base))  in
    take size 0 (loop n)  in
  
  let to_string alphabet size n = 
    let base = List.length alphabet in
    let num_base = to_base_n base size n in
    implode (List.map (fun i -> List.nth alphabet i) num_base) in
  
  if n < 0 then ()
  else
    let print_str s = if s = "" then print_string "  <epsilon>\n"
  	              else print_string ("  "^s^"\n")  in
    let rec loop i = 
        if i <= n then 
  	  let ts = to_string fa.alphabet i  in
  	  let bound = expt (List.length fa.alphabet) i in
  	  let rec loop2 j = 
  	    if j < bound then (if accept(fa,ts j) 
                                 then print_str (ts j)
                               else ();
  			       loop2 (j+1))
  	    else ()  in
  	  (loop2 0; loop (i+1))
        else ()  in
    loop 0


(* 
 * Tester functions that dump the language accepted by a
 * finite automaton, either deterministic or not
 *
 *)
 
let langDFA x = langFA acceptDFA x
let langNFA x = langFA acceptNFA x

