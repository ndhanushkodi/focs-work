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


let isAccepting dfa s = 
  List.fold_right (fun x ans -> x=s) dfa.accepting false



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
(*val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b

List.fold_right f [a1; ...; an] b is f a1 (f a2 (... (f an b) ...)). Not tail-recursive.*)

let steps dfa q syms = 
  List.fold_right (fun x ans-> dfa.delta ans x) syms q







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

let acceptDFA dfa input = 
  isAccepting dfa (steps dfa dfa.start (explode input))



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

let at_least n p xs =  
  List.length(List.filter p xs) >= n 


(*
Code a function max_positive of type 
int list -> int where max_positive xs 
returns the maximum positive element in 
list xs if one exists, and 0 otherwise.

# max_positive [];;
- : int = 0
# max_positive [4];;
- : int = 4
# max_positive [4;5];;
- : int = 5
# max_positive [5;4];;
- : int = 5
# max_positive [4;6;5];;
- : int = 6
# max_positive [-1;-2;-3];;
- : int = 0

*)
(*val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b

List.fold_right f [a1; ...; an] b is f a1 (f a2 (... (f an b) ...)). Not tail-recursive.*)

let max_positive xs =  
  List.fold_right (fun x ma-> if x>ma & x>0 then x else ma) xs 0 

(*

Code a function map_funs of type 
('a -> 'b) list -> 'a -> 'b list where 
map_funs fs x returns the results of 
applying every function in fs to x.

# 
val dbl : int -> string = 

val neg : int -> string = 

# map_funs [] 3;;
- : 'a list = []
# map_funs [dbl] 3;;
- : string list = ["double of 3"]
# map_funs [dbl;neg] 3;;
- : string list = ["double of 3"; "negation of 3"]
# map_funs [dbl;neg;dbl] 3;;
- : string list = ["double of 3"; "negation of 3"; "double of 3"]
# map_funs [(fun x -> x * 2); (fun x -> x * x)] 10;;
- : int list = [20; 100]
# map_funs [(fun x -> "+"^x); (fun x -> "-"^x)] "hello";;
- : string list = ["+hello"; "-hello"]

*)
(*val map : ('a -> 'b) -> 'a list -> 'b list

List.map f [a1; ...; an] applies function f to a1, ..., an, and 
builds the list [f a1; ...; f an] with the results returned by f. Not tail-recursive.*)
let dbl x = "double of "^(string_of_int x)
let neg x = "negation of "^(string_of_int x)

let map_funs fs x =  
  List.map (fun f-> f x) fs 



(*
Code a function map_cross of type 
('a -> 'b) list -> 'a list -> 'b list where
 map_cross fs xs returns all the results of applying 
 a function in fs to a value in xs.

# let dbl x = "double of "^(string_of_int x);;
val dbl : int -> string = 
# let neg x = "negation of "^(string_of_int x);;
val neg : int -> string = 

# map_cross [] [];;
- : 'a list = []
# map_cross [] [1;2;3];;
- : 'a list = []
# map_cross [dbl; neg] [];;
- : string list = []
# map_cross [dbl] [3];;
- : string list = ["double of 3"]
# map_cross [dbl] [1;2;3];;
- : string list = ["double of 1"; "double of 2"; "double of 3"]
# map_cross [dbl;neg] [3];;
- : string list = ["double of 3"; "negation of 3"]
# map_cross [dbl;neg] [1;2;3];;
- : string list =
["double of 1"; "negation of 1"; "double of 2"; "negation of 2"; "double of 3"; "negation of 3"]
# map_cross [(fun x -> "+"^x);(fun x -> "-"^x)] ["hello";"world"];;
- : string list = ["+hello"; "-hello"; "+world"; "-world"]

*)

(*val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b

List.fold_right f [a1; ...; an] b is f a1 (f a2 (... (f an b) ...)). Not tail-recursive.*)
let map_cross fs xs =  
  List.fold_right (fun x acc-> (map_funs fs x)@acc) xs []



(*

Code a function all_pairings of type 
'a list -> 'b list -> ('a * 'b) list where 
all_pairings xs ys returns all the ways of pairing 
up an element of xs with an element of ys.

# all_pairings [] [];;
- : ('a * 'b) list = []
# all_pairings [1;2] [];;
- : (int * 'a) list = []
# all_pairings [] ["a";"b";"c"];;
- : ('a * string) list = []
# all_pairings [1] ["a";"b";"c"];;
- : (int * string) list = [(1, "a"); (1, "b"); (1, "c")]
# all_pairings [1;2] ["a"];;
- : (int * string) list = [(1, "a"); (2, "a")]
# all_pairings [1;2] ["a";"b";"c"];;
- : (int * string) list =
[(1, "a"); (1, "b"); (1, "c"); (2, "a"); (2, "b"); (2, "c")]

*)
let all_pairings xs ys =  
  List.fold_right (fun x acc -> (List.map (fun y -> (x,y)) ys)@acc) xs []





(* QUESTION 3 *)

(*

Code a function prefixes of type 
'a list -> 'a list list where prefixes xs 
returns the list of all prefixes of xs: if xs 
is [x1; x2; x3] then the prefixes of xs are [], [x1], [x1; x2], 
and [x1; x2; x3]. (Note that the empty list is a prefix of every list.)

# prefixes [];;
- : 'a list list = [[]]
# prefixes [1];;
- : int list list = [[]; [1]]
# prefixes [1;2;3;4];;
- : int list list = [[]; [1]; [1; 2]; [1; 2; 3]; [1; 2; 3; 4]]
# prefixes ["a";"b"];;
- : string list list = [[]; ["a"]; ["a"; "b"]]

*)
let prefixes xs =  
  List.fold_right (fun x acc -> acc::[x]) xs [[]]


let suffixes xs =  failwith "suffixes not implemented"


let inject a xs =  failwith "inject not implemented"


let permutations xs =  failwith "permutations not implemented"

