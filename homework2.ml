(* 

HOMEWORK 2

Name: Nitya Dhanushkodi 

Email: nitya.dhanushkodi@students.olin.edu  

Remarks, if any: Sometimes Cynthia and I would help each other
debug code. 

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



(* QUESTION 1 *) (*Order in returning elements is irrelevant*)

(*Code a function prepend of type 
string * string list -> string list which 
takes a string s and a list of strings ss 
and returns the list of strings obtained by 
prepending s to every string in ss.

# prepend("",[]);;
- : string list = []
# prepend("",["hello";"world"]);;
- : string list = ["hello"; "world"]
# prepend("test",[]);;
- : string list = []
# prepend("test",["hello";"world"]);;
- : string list = ["testhello"; "testworld"]

*)
let rec prepend (letter, lang) = 
  match lang with [] -> lang
    |first::rest -> (letter ^ first)::prepend(letter, rest)

(*Code a function concatenate of 
type string list * string list -> string list 
which takes a list of strings ss1 and 
a list of strings ss2 and returns the 
list of strings obtained by concatening every 
string from ss1 with every string in ss2. Function 
prepend might be useful.

# concatenate([],[]);;
- : string list = []
# concatenate([],["hello";"world"]);;
- : string list = []
# concatenate(["a"],["hello";"world"]);;
- : string list = ["ahello"; "aworld"]
# concatenate(["a";"b"],["hello";"world"]);;
- : string list = ["ahello"; "aworld"; "bhello"; "bworld"]
# concatenate(["a";"b"],[]);;
- : string list = []
# concatenate(["a";"b"],["hello"]);;
- : string list = ["ahello"; "bhello"]

*)
 
let rec reverse_helper(ls, build) = 
   match ls with [] -> build
   |first::rest -> reverse_helper(rest, first::build)

let rec append_helper (xs,ys) = 
   match xs with [] -> ys
      |first::rest -> append_helper(rest,first::ys )

let append (xs,ys)=
   append_helper(reverse_helper(xs, []), ys)

let rec concatenate (alphabet, lang) = 
  match alphabet with [] -> []
    |first::rest -> append(prepend(first, lang) ,concatenate(rest,lang))


(*Code a function all_strings of type 
string list * int -> string list where 
all_strings (alph,n) returns the list of 
all strings of length up to n over alphabet alpha. 
You may assume that the alphabet is made up only 
of strings of length 1. (That is, it's fine if 
your code doesn't match the specification when the 
alphabet has longer strings in it.) Function 
concatenate might be useful.

# all_strings([],4);;
- : string list = [""]
# all_strings(["a"],4);;
- : string list = [""; "a"; "aa"; "aaa"; "aaaa"]
# all_strings(["a";"b"],4);;
- : string list =
[""; "a"; "aa"; "aaa"; "aaaa"; "aaab"; "aab"; "aaba"; "aabb"; "ab"; "aba";
 "abaa"; "abab"; "abb"; "abba"; "abbb"; "b"; "ba"; "baa"; "baaa"; "baab";
 "bab"; "baba"; "babb"; "bb"; "bba"; "bbaa"; "bbab"; "bbb"; "bbba"; "bbbb"]
# all_strings(["a";"b";"c"],4);;
- : string list =
[""; "a"; "aa"; "aaa"; "aaaa"; "aaab"; "aaac"; "aab"; "aaba"; "aabb"; "aabc";
 "aac"; "aaca"; "aacb"; "aacc"; "ab"; "aba"; "abaa"; "abab"; "abac"; "abb";
 "abba"; "abbb"; "abbc"; "abc"; "abca"; "abcb"; "abcc"; "ac"; "aca"; "acaa";
 "acab"; "acac"; "acb"; "acba"; "acbb"; "acbc"; "acc"; "acca"; "accb";
 "accc"; "b"; "ba"; "baa"; "baaa"; "baab"; "baac"; "bab"; "baba"; "babb";
 "babc"; "bac"; "baca"; "bacb"; "bacc"; "bb"; "bba"; "bbaa"; "bbab"; "bbac";
 "bbb"; "bbba"; "bbbb"; "bbbc"; "bbc"; "bbca"; "bbcb"; "bbcc"; "bc"; "bca";
 "bcaa"; "bcab"; "bcac"; "bcb"; "bcba"; "bcbb"; "bcbc"; "bcc"; "bcca";
 "bccb"; "bccc"; "c"; "ca"; "caa"; "caaa"; "caab"; "caac"; "cab"; "caba";
 "cabb"; "cabc"; "cac"; "caca"; "cacb"; "cacc"; "cb"; "cba"; "cbaa"; "cbab";
 "cbac"; "cbb"; "cbba"; "cbbb"; "cbbc"; "cbc"; "cbca"; "cbcb"; "cbcc"; "cc";
 "cca"; "ccaa"; "ccab"; "ccac"; "ccb"; "ccba"; "ccbb"; "ccbc"; "ccc"; "ccca";
 "cccb"; "cccc"]
# all_strings(["a";"b"],1);;
- : string list = [""; "a"; "b"]
# all_strings(["a";"b"],0);;
- : string list = [""]
#setEqual(all_strings(["a";"b";"c"],4), [""; "a"; "aa"; "aaa"; "aaaa"; "aaab"; "aaac"; "aab"; "aaba"; "aabb"; "aabc";
 "aac"; "aaca"; "aacb"; "aacc"; "ab"; "aba"; "abaa"; "abab"; "abac"; "abb";
 "abba"; "abbb"; "abbc"; "abc"; "abca"; "abcb"; "abcc"; "ac"; "aca"; "acaa";
 "acab"; "acac"; "acb"; "acba"; "acbb"; "acbc"; "acc"; "acca"; "accb";
 "accc"; "b"; "ba"; "baa"; "baaa"; "baab"; "baac"; "bab"; "baba"; "babb";
 "babc"; "bac"; "baca"; "bacb"; "bacc"; "bb"; "bba"; "bbaa"; "bbab"; "bbac";
 "bbb"; "bbba"; "bbbb"; "bbbc"; "bbc"; "bbca"; "bbcb"; "bbcc"; "bc"; "bca";
 "bcaa"; "bcab"; "bcac"; "bcb"; "bcba"; "bcbb"; "bcbc"; "bcc"; "bcca";
 "bccb"; "bccc"; "c"; "ca"; "caa"; "caaa"; "caab"; "caac"; "cab"; "caba";
 "cabb"; "cabc"; "cac"; "caca"; "cacb"; "cacc"; "cb"; "cba"; "cbaa"; "cbab";
 "cbac"; "cbb"; "cbba"; "cbbb"; "cbbc"; "cbc"; "cbca"; "cbcb"; "cbcc"; "cc";
 "cca"; "ccaa"; "ccab"; "ccac"; "ccb"; "ccba"; "ccbb"; "ccbc"; "ccc"; "ccca";
 "cccb"; "cccc"]);;
- : bool = true     
*)
let rec setIn (e,xs) = 
   match xs with [] -> false
      |first::rest -> if e=first then true else setIn(e,rest)

let rec setSub (xs,ys) = 
   match xs with [] -> true
      |first::rest -> if setIn(first,ys) then setSub(rest, ys) else false

let setEqual (xs,ys) = 
   if setSub(xs,ys) && setSub(ys,xs) then true else false

let rec all_strings_helper (orig_alpha, alphabet, n, build) = 
  if n = 0 then [""] 
  else
    if n = 1 then append([""],append(orig_alpha,build))
    else all_strings_helper(orig_alpha, concatenate(orig_alpha, alphabet), n-1,append(concatenate(orig_alpha, alphabet),build) )

let rec all_strings (alphabet, n) = 
  all_strings_helper(alphabet,alphabet, n, [])

(* let rec all_strings(alphabet,n) = 
  if n = 0 then [""] else concatenate(all_strings(alphabet,n-1),alphabet) *)




(* QUESTION 2 *)

(* Code a function restrict of type 
string list * int -> string list that 
takes a language ss and an integer n and 
returns the language of all strings in ss of length 
at most n. Function String.length in the OCaml 
built-in library might be useful.

# restrict([],4);;
- : string list = []
# restrict(["a";"b"],4);;
- : string list = ["a"; "b"]
# restrict(["a";"b"],0);;
- : string list = []
# restrict(["a";"b"],1);;
- : string list = ["a"; "b"]
# restrict(["a";"b";"abc"],1);;
- : string list = ["a"; "b"]
# restrict(["a";"b";"abc"],2);;
- : string list = ["a"; "b"]
# restrict(["a";"b";"abc"],3);;
- : string list = ["a"; "b"; "abc"]

*)

let rec restrict (xs,n) = 
  match xs with [] -> xs
    |first::rest -> if String.length first <= n then first::restrict(rest,n) else restrict(rest,n)

(* Code a function langUnion of type 
string list * string list * int -> string list 
that takes two languages and an integer n and 
returns the language of all strings in either 
languages of length at most n.

# langUnion([],[],4);;
- : string list = []
# langUnion(["a";"b"],["c";"d"],4);;
- : string list = ["a"; "b"; "c"; "d"]
# langUnion(["a";"b"],["abc";"abcd";"abcde"],4);;
- : string list = ["a"; "b"; "abc"; "abcd"]
# langUnion(["abc";"abcd";"abcde"],["a";"b"],4);;
- : string list = ["abc"; "abcd"; "a"; "b"]
# langUnion(["abc";"abcd";"abcde"],[],4);;
- : string list = ["abc"; "abcd"]
# langUnion([],["abc";"abcd";"abcde"],4);;
- : string list = ["abc"; "abcd"]

*)
let rec setUnion_helper (xs,ys,union_b) =
   match xs with [] -> append(ys, union_b)
      |first::rest -> if setIn(first, union_b) then setUnion_helper(rest, ys, union_b) else setUnion_helper(rest, ys, first::union_b)

let setUnion (xs,ys) = 
   setUnion_helper(xs,ys,[])

let langUnion (xs,ys,n) = 
  restrict(setUnion(xs,ys), n)



(* Code a function langConcat of type string list * string list * int -> string list that takes two languages ss1 and ss2 and an integer n and returns the language of all strings of length at most n obtained by concatenating a string from ss1 to a string from ss2.

# langConcat([],[],4);;
- : string list = []
# langConcat(["a";"b"],[],4);;
- : string list = []
# langConcat([],["c";"d"],4);;
- : string list = []
# langConcat(["a";"b"],["c";"d"],4);;
- : string list = ["ac"; "ad"; "bc"; "bd"]
# langConcat(["ab";"abb"],["c";"cc";"ccc"],4);;
- : string list = ["abc"; "abcc"; "abbc"]

*)
let langConcat (xs,ys,n) = 
  restrict(concatenate(xs,ys),n)


let langStar (xs,n) = 
  restrict(all_strings(xs,n),n)



(* QUESTION 3 *)


(* some helper code -- vaguely advanced OCaml in here, but barely *)

type re = Empty | Unit | Letter of string | Plus of re * re | Times of re * re | Star of re

let lang (s,n) = 
  let fromChar c = String.make 1 c in
  let explode s = 
    let rec loop i result = 
      if i < 0 then result
      else loop (i-1) (s.[i]::result) in
    loop (String.length s - 1) []  in
  (* Grammar: 
   *
   * R ::= R1 + R
   *       R1
   * 
   * R1 ::= R2 R1
   *        R2
   * 
   * R2 ::= R3*
   *        R3
   * 
   * R3 ::= a
   *        1
   *        0 
   *        ( R )
   *)
  let isalpha = function 'A'..'Z'|'a'..'z' -> true | _ -> false in
  let expect c cs = 
    match cs with 
      f::cs when f = c -> Some cs
    | _ -> None in
  let expect_alpha cs = 
    match cs with
      f::cs when isalpha f -> Some (f,cs)
    | _ -> None  in
  let rec parse_R cs = 
    match parse_R1 cs with
      None -> None
    | Some (r1,cs) -> 
        (match expect '+' cs with
           None -> Some (r1,cs)
         | Some cs -> 
             (match parse_R cs with
                None -> None
              | Some (r2,cs) -> Some (Plus(r1,r2),cs)))
  and parse_R1 cs = 
    match parse_R2 cs with
      None -> None
    | Some (r1,cs) -> 
        (match parse_R1 cs with
           None -> Some (r1,cs)
         | Some (r2,cs) -> Some (Times(r1,r2),cs))  
  and parse_R2 cs = 
    match parse_R3 cs with
      None -> None
    | Some (r1,cs) -> 
        (match expect '*' cs with
           None -> Some (r1,cs)
         | Some cs -> Some (Star(r1),cs))
  and parse_R3 cs = 
    match expect_alpha cs with
      Some (a,cs) -> Some (Letter(fromChar(a)),cs)
    | None -> 
        (match expect '1' cs with
           Some cs -> Some (Unit, cs)
         | None -> 
             (match expect '0' cs with
                Some cs -> Some (Empty,cs)
              | None -> parse_parens cs))
  and parse_parens cs = 
    match expect '(' cs with
      None -> None
    | Some cs -> 
        (match parse_R cs with
           None -> None
         | Some (r,cs) -> 
             (match expect ')' cs with
                None -> None
              | Some cs -> Some (r,cs)))  in
  let parse s = 
    let cs = explode s in
    match parse_R cs with
      Some (re,[]) -> re
    | _ -> failwith ("Cannot parse "^s)  in
  let rec eval re = 
    match re with
      Empty -> []
    | Unit -> [""]
    | Letter (a) -> [a]
    | Plus (r1,r2) -> langUnion(eval r1,eval r2,n)
    | Times (r1,r2) -> langConcat(eval r1,eval r2,n)
    | Star r -> langStar(eval r,n)  in
    eval (parse s)

let dump l = 
  List.iter (fun s -> match s with "" -> print_string "  <empty>\n" 
                                 | s -> print_string ("  "^s^"\n")) l



(* Placeholder for your regular expression. Replace "0" by your actual answer *)

let rec remove_dups lst= match lst with 
  | [] -> []
  | h::t -> h::(remove_dups (List.filter (fun x -> x<>h) t))

let regexp_a = "(a+b)(a+b)(a+b)"

let regexp_b = "((a+b)(a+b)(a+b))*"

let regexp_c = "(b)*a(b)*"

let regexp_d = "b*ab*(b*ab*ab*)*b*"
(*"((b)*a(b)*) + (((b)*a(b)*)(b*ab*ab*)*) + ((b*ab*ab*)*((b)*a(b)*))"*)

let regexp_e = "a*((b+a)(a)*)a*"
