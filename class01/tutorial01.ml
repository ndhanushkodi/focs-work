let rec max_helper (lst, max_sf) = 
	match lst with [] -> max_sf
		|first::rest -> if first>max_sf then max_helper(rest, first) else max_helper(rest, max_sf)

let max (lst) =
	max_helper(lst, 0)

(* let rec max_helper (lst, max_sf) = 
	match lst with [] -> max_sf
		|first::[] -> first
		|first::rest -> if first>max_sf then max_helper(rest, first) else max_helper(rest, max_sf)

let max (lst) =
	max_helper(lst, 0) *)

let merge_help (xs,ys, build, which) = 
	match xs with [] -> build
		|first::rest -> if which=0 then merge_help(rest, ys, first::build, 1) else merge_help(xs,rest,first::build, 0) 

let merge (xs, ys) = 
	merge_help(xs,ys,[], 0)

let rec zip (xs, ys) = 
	match xs with [] -> []
		|firstx::restx ->
			match ys with [] -> []
				|firsty::resty -> firstx::firsty::zip(restx,resty)

let rec setIn (e,xs) = 
   match xs with [] -> false
      |first::rest -> if e=first then true else setIn(e,rest)

let rec compress (lst, prev) = 
	match lst with [] -> lst
		|first::rest -> if first=prev then compress(rest, first) else first::build::compress(rest, first)

let comp (lst) = 
	compress(lst, None)


(* match xs,ys with
	|[],[] -> []
	|xh::xt, yh::yt -> pasdlkjkfka *)

let rec compress lst = 
	match lst with 
		|[] -> []
		|[x] -> [x]
		|[x;y] -> if x=y then [x] else [x;y]
		|x1::x2::rest -> if x1=x2 then compress(x2::rest) else x1::compress(x2::rest)







