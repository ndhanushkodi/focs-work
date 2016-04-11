(* 

HOMEWORK 7

Name: Nitya Dhanushkodi 

Email: nitya.dhanushkodi@students.olin.edu

Remarks, if any: Rewrites of the unrestricted grammars are shown below the grammars for question 3. 

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
 * Type for grammars
 *
 *)

type grammar = {
  nonterminals: string list;
  terminals: string list;
  rules: (string * string) list;
  startsym : string
}


(* 
 * Some sample (context-free) grammars 
 *
 *)

let anbn = {
  nonterminals = ["S"];
  terminals = ["a";"b"];
  rules = [("S","");
           ("S","aSb")];
  startsym = "S"
}

let anbm = {
  nonterminals = ["S";"T";"B"];
  terminals = ["a";"b"];
  rules = [ ("S","TB");
            ("T","");
	    ("T","aTb");
	    ("B","");
	    ("B","Bb")];
  startsym = "S"
}


(*
 * Here's a grammar that is _not_ context-free
 *
 * It's also harder to generate its strings
 *
 *)

let anbncn = {
  nonterminals = ["S";"A";"B";"C";"X"];
  terminals = ["a";"b";"c"];
  rules = [ ("S","");
            ("S","ABC");
	    ("bX","Xb");
	    ("AX","a");
	    ("aX","Xa");
	    ("XC","c");
	    ("Xc","cX");
	    ("B","XbBX");
	    ("B","");
	    ("A","AA");
	    ("C","CC")];
  startsym = "S"
}




(* abbreviations *)

let map = List.map
let len = String.length
let sub = String.sub


(*
 * Utility functions 
 * 
 *)


(* check is lhs is a prefix of str *)

let prefix lhs str =
  lhs = (sub str 0 (len lhs))


(* replace prefix lhs of str with rhs *)

let replace lhs str rhs =
  let l = len lhs in
  rhs ^ (sub str l (len str - l))


(* try to apply rule (lhs,rhs) to str (assuming prefix prf) *)

let apply_rule prf (lhs,rhs) str =
  if len str < len lhs 
    then []
  else if prefix lhs str
    then [prf^(replace lhs str rhs)]
  else []


(* try to apply every rule in rs to str *)

let rec apply_rules rs str =
  let rec loop prefix str = 
    if str = "" then []
    else let rest = loop (prefix^(sub str 0 1)) (sub str 1 (len str -1))  in
       (List.fold_left (fun res r -> res@(apply_rule prefix r str)) [] rs)@rest  in
  loop "" str


(*
 * Perform an iteratively deepening depth-first search of the rewrite 
 * tree
 *
 *)

module StringSet = Set.Make(String)

let dfs_path maxdepth maxwidth grammar target =
  let lt = len target  in
  let rec loop q seen =
    if q = []
      then []
    else let ((path,d)::q) = q in
         let (str::_) = path in
	 if len str > maxwidth
	   then loop q seen
         else if len str = lt && str = target
	   then path
	 else if StringSet.mem str seen
	   then loop q seen
	 else if d > maxdepth
	   then loop q (StringSet.add str seen)
	 else (* let _ = (print_string str; print_newline()) in *)
	      let new_strs = apply_rules grammar.rules str in
	      let new_strs_d = map (fun x -> (x::path,d+1)) new_strs in
	      let q = (new_strs_d)@q in
	      loop q (StringSet.add str seen) in
  loop [([grammar.startsym],0)] StringSet.empty

let idfs_path maxdepth grammar target =
  let rec loop n =
    let _ = Printf.printf "Searching (depth %02d, max width %d)" n n in
    let _ = print_newline ()  in
    if n > maxdepth
      then []
    else match dfs_path n n grammar target with
         | [] -> loop (n+1)
	 | path -> path  in
  loop 1


(* 
 * Check if a grammar is well-formed 
 *
 *)

let checkGrammar grammar = 
  let _ = List.iter (fun x -> if String.length x != 1 then failwith ("symbol "^x^" not a single character") else ()) grammar.nonterminals  in
  let _ = List.iter (fun x -> if String.length x != 1 then failwith ("symbol "^x^" not a single character") else ()) grammar.terminals  in
  let _ = List.iter (fun (p,q) -> if String.length p < 1 then failwith "rule with empty left-hand side" else ()) grammar.rules  in
  let _ = if List.mem grammar.startsym grammar.nonterminals then () else failwith "start symbol not a nonterminal"  in
  ()



(*
 * Try to generate a string for a given grammar 
 * 
 *)

let generate md grammar str =
  let _ = checkGrammar grammar in
  let print pre str = (print_string pre;
                       print_string str;
		       print_newline ())  in
  let rec rev_print path =
    match path with
    | [] -> ()
    | [s] -> print "   " s
    | s::ss -> (rev_print ss; print "-> " s)  in
  let path = idfs_path md grammar str  in
  let _ = rev_print path  in
  path != []
  


(* 
 * QUESTION 1
 *
 *)

(*let anbn = {
  nonterminals = ["S"];
  terminals = ["a";"b"];
  rules = [("S","");
           ("S","aSb")];
  startsym = "S"
}

let anbm = {
  nonterminals = ["S";"T";"B"];
  terminals = ["a";"b"];
  rules = [ ("S","TB");
            ("T","");
      ("T","aTb");
      ("B","");
      ("B","Bb")];
  startsym = "S"
}*)
let palindromes = {
  nonterminals = ["S"];
  terminals = ["a";"b";"c"];
  rules = [("S","");
           ("S","aSa");
           ("S","bSb");
           ("S","cSc");
           ("S","a");
           ("S","b");
           ("S","c")];
  startsym = "S"
} 


let ambncmn = {
  nonterminals = ["S";"T"];
  terminals = ["a";"b";"c"];
  rules = [("S","aSc");
           ("S", "");
           ("S","T");
           ("T","bTc");
           ("T", "")];
  startsym = "S"
} 


let amcmnbn = { 
  nonterminals = ["S"; "T"];
  terminals = ["a";"b";"c"];
  rules = [("S","aScT");
           ("S", "");
           ("T","cTb");
           ("T", "")];
  startsym = "S"
} 


let ambncm = {
  nonterminals = ["S"; "T"];
  terminals = ["a"; "b"; "c"];
  rules = [("S","aSc");
           ("S", "");
           ("S","T");
           ("T", "bT");
           ("T", "")];
  startsym = "S"
} 


let eqnum = {
  nonterminals = ["S"];
  terminals = ["d";"e"];
  rules = [("S","dSe");
           ("S", "eSd");
           ("S","");
           ("S", "SS")];
  startsym = "S"
} 



(* 
 * QUESTION 2 
 *
 *)

(* Type for DFAs *)

type 'a dfa = { states: 'a list;
		alphabet: char list;
		delta: 'a -> char -> 'a;
		start : 'a;
		accepting : 'a list }


(* A dfa that accepts all strings with a multiple of three
 * number of as *)

let dfaThreeA = { 
  states = ["S";"1";"2"];
  alphabet = ['a';'b'];
  delta = (fun q a -> 
             match (q,a) with
	       ("S",'a') -> "1"
	     | ("1",'a') -> "2"
	     | ("2",'a') -> "S"
	     | ("S",'b') -> "S"
	     | ("1",'b') -> "1"
	     | ("2",'b') -> "2"
	     | _ -> "");
  start = "S";
  accepting = ["S"]
} 

let rec pairs xs ys =
  List.fold_right (fun x r -> (List.map (fun y -> (x,y)) ys)@r) xs []

let dTList dfa = pairs dfa.states dfa.alphabet
let dTRules dfa = List.fold_right(fun (s,a) acc -> (s,(Char.escaped(a))^(dfa.delta s a))::acc) (dTList dfa) []

let dfaGrammar dfa = {
  nonterminals = dfa.states;
  terminals = map (fun x -> Char.escaped(x)) dfa.alphabet;
  rules = (dTRules dfa)@(map (fun x -> (x,"")) dfa.accepting);
  startsym = dfa.start
}




(*
 * QUESTION 3 
 *
 *)
  

let addition = {
  nonterminals = ["S"; "M"];
  terminals = ["x"; "+"; "="];
  rules = [("S","+=");
           ("S", "xSx");
           ("S","+M");
           ("M", "xMx");
           ("M","=");
           ("M", "")
           ];
  startsym = "S"
} 
(*  RESULTS
xxx+xx=xxxxx
 S
-> xSx
-> xxSxx
-> xxxSxxx
-> xxx+Mxxx
-> xxx+xMxxxx
-> xxx+xxMxxxxx
-> xxx+xx=xxxxx


xx+xxx=xxxxx
   S
-> xSx
-> xxSxx
-> xx+Mxx
-> xx+xMxxx
-> xx+xxMxxxx
-> xx+xxxMxxxxx
-> xx+xxx=xxxxx

*)



let powers2 = {
  nonterminals = ["S";"C";"D";"X"];
  terminals = ["a"];
  rules = [("S","CaX");
           ("C", "CD");
           ("Da","aaD");
           ("DX", "X");
           ("C","");
           ("X", "")];
  startsym = "S"
} 
(*
aaaa

   S
-> CaX
-> CDaX
-> CDDaX
-> DDaX
-> DaaDX
-> aaDaDX
-> aaaaDDX
-> aaaaDX
-> aaaaX
-> aaaa
- : bool = true


aaaaaaaa

   S
-> CaX
-> CDaX
-> CDDaX
-> CDDDaX
-> DDDaX
-> DDaaDX
-> DaaDaDX
-> aaDaDaDX
-> aaaaDDaDX
-> aaaaDaaDDX
-> aaaaaaDaDDX
-> aaaaaaaaDDDX
-> aaaaaaaaDDX
-> aaaaaaaaDX
-> aaaaaaaaX
-> aaaaaaaa
- : bool = true


*)