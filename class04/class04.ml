


let rec squares(ls) =
	match ls with [] -> []
		|first::rest -> first*first::squares(rest)

 let rec diags(ls) = 
 	match ls with [] -> []
 		|first::rest -> (first,first)::diags(rest)

 let rec lengths(lsts) = 
 	match lsts with [] -> []
 		|first::rest -> List.length(first)::lengths(rest)


(* let square x = x*x
 *)
 let rec map(f,xs) = 
 	match xs with 
 	[] -> []
 	|x::xs' -> (f x)::(map(f, xs'))


(*  let diags xs = map(diags, xs);; *)
(*map((fun x->x*3), [1;2;3;4]);;*)

let diags3 (ls) = map((fun x->(x,x,x)), ls)
let thirds (tupls) = map((fun (x,y,z) -> z), tupls)

(* let thirds(tupls) = map((fun (x,y,z) -> z), tupls) *)

let distribute (y, xs) = map((fun x-> (y,x)), xs)

let distribute_v2 (y,xs) = 
	let mkPair x = (y,x) in
		map(mkPair,xs)

let create_mkPair y = (fun x -> (y,x))

let distribute_v3 (y,xs) = 
	map(create_mkPair y, xs)

let rec removeEmpty (xss) = 
	match xss with 
	[] -> []
	|xs::xss' -> ( if (List.length(xs) >0) then xs::removeEmpty(xss') else removeEmpty(xss') )

let rec filter (p, xs) = 
	match xs with [] -> []
	|first::rest -> if (p first) = true then first::(filter(p,rest)) else filter(p, rest)

let odd n = (n mod 2 = 1)


let rec map_curried f xs = 
	match xs with 
	[] -> []
	|x::xs' -> (f x)::(map_curried f xs')


let rec map_append f xs =
	match xs with
	[] -> []
	| x::xs' -> (f x) @ (map_append f xs')


let rec flatten xss = 
	map_append (fun x -> x)  xss

(*filter odd [1;2;3]*)
let rec filter p xs = 
	map_append (fun x-> if(p x) then [x] else []) xs

let rec map_general1 comb f xs =
	match xs with 
	[] -> []
	|x::xs' -> comb (f x) (map_general1 comb f xs')

let map1 f xs = map_general1 (fun x y -> x::y) f xs
let map_append1 f xs = map_general1(fun x y -> x@y) f xs



let rec map_general comb xs = 
	match xs with 
	[] -> []
	|x::xs' -> comb x (map_general comb xs')

let map f xs = map_general (fun x y -> (f x)::y) xs
let map_append f xs = map_general (fun x y -> (f x)@y) xs

let rec sum xs =
	match xs with 
	[] -> 0
	|x::xs' -> x + sum xs' (*BUT MAP GENERAL RETURNS A LIST!*)

let rec sum_general comb xs =
	match xs with 
	[] -> 0
	|x::xs' -> comb x (sum_general comb xs')
(*let new_sum xs = sum_general(fun x y -> x+y) xs*)


(*isolated recursion over a list*)
let rec fold_right comb xs base =
	match xs with 
	[] -> base
	|x::xs' -> comb x (fold_right comb xs' base)
(*general(fun x y -> x+y) [1;2;3;4;5] 0;;*)
(*general(fun x y -> (x+1)::y) [1;2;3;4;5] [2;3];;*) (*map*)