(* 

HOMEWORK 1

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
 * It has to load without any errors.
 *
 *)



(* Question 1 *)

(*Code a function gcd of type int * int -> int 
which takes two integers and returns the 
greatest common divisor of those integers.

# gcd (1,1);;
- : int = 1
# gcd (1,3);;
- : int = 1
# gcd (2,4);;
- : int = 2
# gcd (4,2);;
- : int = 2
# gcd (4,6);;
- : int = 2
*)
let rec gcd (a,b) = 
   if b = 0
      then a
   else
      if a>b then gcd(b,a mod b) else gcd(a,b mod a);;
      

     
(*Two integers are coprime if they have 
only the trivial divisor in common — that is, 
if their greatest common divisor is 1.
Code a function is_coprime of type int * int -> bool 
which returns true if the two integers are coprime, 
and false otherwise.

# is_coprime (1,2);;
- : bool = true
# is_coprime (2,3);;
- : bool = true
# is_coprime (2,4);;
- : bool = false
# is_coprime (10,20);;
- : bool = false
# is_coprime (9,16);;
- : bool = true
*)
let is_coprime (a,b) = 
   if gcd(a,b) = 1
      then true
   else
      false;;


(*The Euler φ function is defined by taking φ(n) 
to be the number of integers 1≤ x ≤ n such 
that x and n are coprime. Code a function euler 
of type int -> int which computes φ(n) of its input n.

# euler 1;;
- : int = 1
# euler 2;;
- : int = 1
# euler 3;;
- : int = 2
# euler 4;;
- : int = 2
# euler 10;;
- : int = 4
# euler 20;;
- : int = 8
# euler 5555;;
- : int = 4000
*)
let rec euler_helper (co_b, num_at, n) = 
   if num_at > n
      then co_b
   else 
      if is_coprime(num_at, n) 
         then euler_helper(co_b+1, num_at+1, n)
      else euler_helper(co_b, num_at+1, n);;

let euler (n) = euler_helper(0,1, n)
   

(*Code a function coprimes of type 
int -> int list which returns the list 
of all integers 1≤x≤n such that x and n are coprime.

# coprimes 1;;
- : int list = [1]
# coprimes 2;;
- : int list = [1]
# coprimes 3;;
- : int list = [1; 2]
# coprimes 4;;
- : int list = [1; 3]
# coprimes 10;;
- : int list = [1; 3; 7; 9]
# coprimes 20;;
- : int list = [1; 3; 7; 9; 11; 13; 17; 19]
# coprimes 5555;;
- : int list =
[1; 2; 3; 4; 6; 7; 8; 9; 12; 13; 14; 16; 17; 18; 19; 21; 23; 24; 26; 27; 28;
 29; 31; 32; 34; 36; 37; 38; 39; 41; 42; 43; 46; 47; 48; 49; 51; 52; 53; 54;
 56; 57; 58; 59; 61; 62; 63; 64; 67; 68; 69; 71; 72; 73; 74; 76; 78; 79; 81;
 82; 83; 84; 86; 87; 89; 91; 92; 93; 94; 96; 97; 98; 102; 103; 104; 106; 107;
 108; 109; 111; 112; 113; 114; 116; 117; 118; 119; 122; 123; 124; 126; 127;
 128; 129; 131; 133; 134; 136; 137; 138; 139; 141; 142; 144; 146; 147; 148;
 149; 151; 152; 153; 156; 157; 158; 159; 161; 162; 163; 164; 166; 167; 168;
 169; 171; 172; 173; 174; 177; 178; 179; 181; 182; 183; 184; 186; 188; 189;
 191; 192; 193; 194; 196; 197; 199; 201; 203; 204; 206; 207; 208; 211; 212;
 213; 214; 216; 217; 218; 219; 221; 222; 223; 224; 226; 227; 228; 229; 232;
 233; 234; 236; 237; 238; 239; 241; 243; 244; 246; 247; 248; 249; 251; 252;
 254; 256; 257; 258; 259; 261; 262; 263; 266; 267; 268; 269; 271; 272; 273;
 274; 276; 277; 278; 279; 281; 282; 283; 284; 287; 288; 289; 291; 292; 293;
 294; 296; 298; 299; 301; 302; 304; 306; 307; 309; 311; 312; 313; 314; 316;
 317; 318; 321; 322; 323; 324; 326; 327; 328; 329; 331; 332; 333; 334; 336;
 337; 338; 339; 342; 343; 344; 346; 347; 348; 349; 351; 353; 354; 356; 357;
 358; 359; 361; 362; 364; 366; 367; 368; 369; 371; 372; 373; 376; 377; 378;
 379; 381; 382; 383; 384; 386; 387; 388; 389; 391; 392; 393; 394; 397; 398;
399; 401; 402; 403; 406; 408; 409; 411; 412; 413; 414; 416; ...]

*)
let rec coprimes_helper (co_b_list, num_at, n)=
   if num_at = 0
      then co_b_list 
   else 
      if is_coprime(num_at, n) 
         then coprimes_helper(num_at::co_b_list, num_at-1, n)
      else coprimes_helper(co_b_list, num_at-1, n);;

let coprimes (n) = 
   coprimes_helper([], n,n)



(* Question 2 *)

(* Code a function append of type 
'a list * 'a list -> 'a list which takes two 
lists and returns a new list consisting of the 
second list appended at the end of the first.

# append ([],[]);;
- : 'a list = []
# append ([1],[]);;
- : int list = [1]
# append ([],[1]);;
- : int list = [1]
# append ([1],[1]);;
- : int list = [1; 1]
# append ([1;2;3],[4;5;6]);;
- : int list = [1; 2; 3; 4; 5; 6]
# append (["a"],["b"]);;
- : string list = ["a"; "b"]

*)
let rec last_helper(ls) = 
   match ls with [] -> None
      |[x] -> Some x
      | first::rest -> last_helper(rest)

let rec reverse_helper(ls, build) = 
   match ls with [] -> build
   |first::rest -> reverse_helper(rest, first::build)

let rec append_helper (xs,ys) = 
   match xs with [] -> ys
      |first::rest -> append_helper(rest,first::ys )

let append (xs,ys)=
   append_helper(reverse_helper(xs, []), ys)



(* Code a function flatten of type 
'a list list -> 'a list which takes a 
list of lists and "flattens" it into a single list.

# flatten [];;
- : 'a list = []
# flatten [[1;2;3]];;
- : int list = [1; 2; 3]
# flatten [[1;2;3];[4;5;6]];;
- : int list = [1; 2; 3; 4; 5; 6]
# flatten [[1;2;3];[4;5;6];[7;8]];;
- : int list = [1; 2; 3; 4; 5; 6; 7; 8]
# flatten [[1;2;3];[];[7;8]];;
- : int list = [1; 2; 3; 7; 8]
# flatten [["a"];["b"]];;
- : string list = ["a"; "b"]

*)

let rec flatten_helper (xss, build) =
   match xss with [] -> build
      |first::rest -> flatten_helper(rest, append(first,build)) 

let flatten (xss) = 
   flatten_helper(reverse_helper(xss,[]),[])

(*Code a function nth of type 
int * 'a list -> 'a where nth(n,xs) returns 
the element at position n in list xs, 
where 0 is the position of the first element.

If the position input n is out of bounds, 
use built-in function failwith to return an error.

# nth (0,["a";"b";"c"]);;
- : string = "a"
# nth (1,["a";"b";"c"]);;
- : string = "b"
# nth (2,["a";"b";"c"]);;
- : string = "c"
# nth (0,["a";"b";"c"]);;
- : string = "a"
# nth (3,["a";"b";"c"]);;
Exception: Failure "out of bounds".
# nth (0,[]);;
Exception: Failure "out of bounds".

*)
let rec nth_helper (n,count, num_at_count, xs)=
   if n = count then num_at_count
   else
      match xs with [] -> failwith "out of bounds"
         |first::rest -> nth_helper(n, count+1, first, rest)

let nth (n,xs) = 
   match xs with [] -> failwith "out of bounds"
      |first::rest -> nth_helper(n,0,first,rest)


let last (xs) = 
   failwith "not implemented"


let separate (xs) = 
   failwith "not implemented"



(* Question 3 *)

let setIn (e,xs) = 
   failwith "not implemented"


let setSub (xs,ys) = 
   failwith "not implemented"


let setEqual (xs,ys) = 
   failwith "not implemented"


let setUnion (xs,ys) = 
   failwith "not implemented"


let setInter (xs,ys) = 
   failwith "not implemented"


let setSize (xs) = 
   failwith "not implemented"

