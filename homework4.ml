(*************************************************** 

HOMEWORK 4

Name: 

Email:

Remarks, if any:

***************************************************)



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
 *   explode : string -> char list
 *      returns the list of characters making up a string
 *
 *   implode : char list -> string
 *      concatenates the list of characters into a string
 *
 *)

let explode str = 
  let rec acc index result = 
    if (index<0) then result
    else acc (index-1) ((String.get str index)::result) in
  acc (String.length(str)-1) []

let implode cs = 
  List.fold_right (fun a r -> (String.make 1 a)^r) cs ""



(*
 *  The type of a DETERMINISTIC finite automaton
 * 
 *  Note that the delta here is _actually_ a function
 *  from states and symbols to states
 * 
 *)

type 'a dfa = { states: 'a list;
		alphabet: char list;
		delta: 'a -> char -> 'a;
		start : 'a;
		accepting : 'a list }


(*
 *  A sample DFA that accepts all strings over
 *  {a,b} with a multiple-of-3 number of a's
 *
 *)

let dfaThreeA = { 
  states = ["start";"one";"two"];
  alphabet = ['a';'b'];
  delta = (fun q a -> 
             match (q,a) with
	       ("start",'a') -> "one"
	     | ("one",'a') -> "two"
	     | ("two",'a') -> "start"
	     | ("start",'b') -> "start"
	     | ("one",'b') -> "one"
	     | ("two",'b') -> "two");
  start = "start";
  accepting = ["start"]
} 




(* QUESTION 1 *)

(*
Code a function isAccepting of 
type 'a dfa -> 'a -> bool which takes a deterministic 
finite automaton m (of the type dfa above) and 
a state q of m and returns true when q is an accepting 
states and false otherwise.

Note the type of the function. It should be a curried function,
 like every other function in this homework.

# isAccepting dfaThreeA "start";;
- : bool = true
# isAccepting dfaThreeA "one";;
- : bool = false
# isAccepting dfaThreeA "two";;
- : bool = false

*)
let rec setIn e xs = 
   match xs with [] -> false
      |first::rest -> if e=first then true else setIn e rest

let isAccepting dfa s = 
  setIn s dfa.accepting



(*Code a function steps of type 
'a dfa -> 'a -> char list -> 'a which 
takes a deterministic finite automaton 
m, a state q of m, and a list of symbols l 
of the alphabet of m, and returns the state of m 
resulting from following the transitions of m from q
according to the symbols in l.

# steps dfaThreeA "start" [];;
- : string = "start"
# steps dfaThreeA "start" ['a'];;
- : string = "one"
# steps dfaThreeA "start" ['a';'b'];;
- : string = "one"
# steps dfaThreeA "start" ['a';'b';'a'];;
- : string = "two"
# steps dfaThreeA "one" [];;
- : string = "one"
# steps dfaThreeA "one" ['a'];;
- : string = "two"
# steps dfaThreeA "one" ['a';'b'];;
- : string = "two"
# steps dfaThreeA "one" ['a';'b';'a'];;
- : string = "start"

*)
let rec findTransitions_helper (delta,q,a) = 
  match delta with [] -> []
    | (x,y,z)::rest -> if x=q & y=a then (x,y,z)::findTransitions_helper(rest,q,a) else findTransitions_helper(rest,q,a)

let findTransitions (fa,q,a) = 
  findTransitions_helper(fa.delta, q, a)

let step (fa,q,a) = 
  match findTransitions(fa,q,a) with [] -> failwith "no step"
    |(x,y,z)::[] -> z

let steps dfa q syms = 
  match syms with [] -> q
    |first::rest -> steps fa step(fa,q,first) rest




(*Code a function acceptDFA of type 
'a dfa -> string -> bool which takes a
deterministic finite automaton m and a string s 
over the alphabet of m, and returns true if m 
accepts s, and false otherwise.

You can use function explode (provided in the
homework code) to turn a string into a list of characters.

For testing purposes, I've provided you also with 
a function langDFA where langDFA m k prints out all 
the strings of length up to k accepted by the 
deterministic finite automaton m. Note that it 
calls your function acceptDFA, so it won't work 
correctly until you implement that.

# acceptDFA dfaThreeA "";;
- : bool = true
# acceptDFA dfaThreeA "a";;
- : bool = false
# acceptDFA dfaThreeA "b";;
- : bool = true
# acceptDFA dfaThreeA "aa";;
- : bool = false
# acceptDFA dfaThreeA "aaa";;
- : bool = true
# acceptDFA dfaThreeA "ababa";;
- : bool = true
# acceptDFA dfaThreeA "abababa";;
- : bool = false
# langDFA dfaThreeA 6;;*)
let acceptDFA dfa input = failwith "acceptDFA not implemented"



(* This function loops through all the strings
 * of length up to n, and prints those that are accepted by the
 * finite automaton.
 *
 * This is basically the same as in the last homework
 *)

let langDFA dfa n = 
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
	let ts = to_string dfa.alphabet i  in
  	let bound = expt (List.length dfa.alphabet) i in
  	let rec loop2 j = 
  	  if j < bound then (if acceptDFA dfa (ts j) 
                               then print_str (ts j)
                             else ();
  			     loop2 (j+1))
  	  else ()  in
  	(loop2 0; loop (i+1))
      else ()  in
      loop 0





(* QUESTION 2 *)

(*For each function you have to code in this question, 
full points will be awarded if the function does not
 use explicit recursion — that is, if you use List.map
  or List.filter or List.fold_right. If you can't 
  get it to work without recursion, please provide 
  a version with explicit recursion.

    The OCaml List module contains functions 
    for_all and exists that both take a 
    predicate and a list, and return true if 
    all the elements of the list satisfy the 
    predicate (for for_all), or at least one 
    element of the list satisfies the predicate 
    (for exists).

    Code a function at_least of type 
    int -> ('a -> bool) -> 'a list -> bool 
    where at_least n p xs returns true exactly 
    when there are at least n elements in xs 
    that satisfy predicate p.

    # at_least 0 (fun x -> x) [];;
    - : bool = true
    # at_least 1 (fun x -> x) [];;
    - : bool = false
    # at_least 0 (fun x -> x) [true;true;false];;
    - : bool = true
    # at_least 1 (fun x -> x > 0) [2;3;0];;
    - : bool = true
    # at_least 2 (fun x -> x > 0) [2;3;0];;
    - : bool = true
    # at_least 3 (fun x -> x > 0) [2;3;0];;
    - : bool = false

*)

let at_least n p xs =  failwith "at_least not implemented"

let max_positive xs =  failwith "max_positive not implemented"


let map_funs fs x =  failwith "map_funs not implemented"


let map_cross fs xs =  failwith "map_cross not implemented"


let all_pairings xs ys =  failwith "all_pairings not implemented"





(* QUESTION 3 *)


let prefixes xs =  failwith "prefixes not implemented"


let suffixes xs =  failwith "suffixes not implemented"


let inject a xs =  failwith "inject not implemented"


let permutations xs =  failwith "permutations not implemented"

