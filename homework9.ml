(* 

HOMEWORK 9

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



(* The underlying implementation for streams 
 *
 * Basically, a stream is a pair of an element and a 
 * "promise" to compute the rest of the stream.
 * 
 * That "promise" is represented as a function
 *
 * The implementation memoizes that function: once the function is
 * called once, it remembers its result, and subsequent calls to the
 * function directly return the result without executing the body of
 * the function
 *
 * You don't need to know anything about this code -- you will
 * use functions fby, head, and tail described below instead
 *)

module AbsStream :
  sig
      type 'a stream 
      val mk : 'a -> (unit -> 'a stream) -> 'a stream 
      val unmk1 : 'a stream -> 'a 
      val unmk2 : 'a stream -> 'a stream
      val cst : 'a -> 'a stream
      val fby : 'a stream -> (unit -> 'a stream) -> 'a stream
      val map : ('a -> 'b) -> 'a stream -> 'b stream
      val filter : ('a -> 'b -> bool) -> 'a stream -> 'b stream -> 'b stream
      val split : 'a stream -> ('a stream * 'a stream)
      val zip : 'a stream -> 'b stream -> ('a * 'b) stream
      val prefix : int -> 'a stream -> 'a list
      val nth : int -> 'a stream -> 'a
    end = 
  struct
    
    type 'a stream = R of 'a * (unit -> 'a stream)
	  
    let memoize f = 
      let memoized = ref None in
      let new_f () = 
	match !memoized with
	| None -> let result = f () in memoized := Some result; result
	| Some v -> v   in
      new_f
	
    let mk h t = R (h, memoize t) 
    let unmk1 s = let R (h,t) = s in h
    let unmk2 s = let R (h,t) = s in t ()

    let rec cst v = mk v (fun () -> cst v)
    let fby s1 ps2 = mk (unmk1 s1) ps2
    let rec map f s = mk (f (unmk1 s)) (fun () -> map f (unmk2 s))
    let rec filter p ctl s = if p (unmk1 ctl) (unmk1 s) then mk (unmk1 s) (fun () -> filter p (unmk2 ctl) (unmk2 s)) else filter p (unmk2 ctl) (unmk2 s)
    let split s = (cst (unmk1 s), unmk2 s)
    let rec zip s1 s2 = mk (unmk1 s1, unmk1 s2) (fun () -> zip (unmk2 s1) (unmk2 s2))

    let rec prefix n s = if n > 0 then (unmk1 s)::(prefix (n-1) (unmk2 s)) else []
    let rec nth n s = if n > 0 then nth (n-1) (unmk2 s) else unmk1 s

  end


(*
 * These are the stream functions you will use
 *
 *)

type 'a stream = 'a AbsStream.stream

let cst : 'a -> 'a stream = AbsStream.cst
        (* constant *)

let fby : 'a stream -> (unit -> 'a stream) -> 'a stream = AbsStream.fby
        (* followed by *)

let map : ('a -> 'b) -> 'a stream -> 'b stream = AbsStream.map
        (* map a function over a stream *)

let filter : ('a -> 'b -> bool) -> 'a stream -> 'b stream -> 'b stream = AbsStream.filter
           (* filter a stream based on a control stream and a predicate *)

let zip : 'a stream -> 'b stream -> ('a * 'b) stream = AbsStream.zip
        (* zip two streams into a stream of pairs *)

let split : 'a stream -> ('a stream * 'a stream) = AbsStream.split
          (* split a stream into two streams *)

let prefix : int -> 'a stream -> 'a list = AbsStream.prefix
           (* return the first n elements of a stream *)

let nth : int -> 'a stream -> 'a = AbsStream.nth
           (* return the nth element of a stream *)



(* some useful sample streams, from class *)

let nats =
  let rec natsF () = fby (cst 0)
                         (fun () -> (map (fun x -> x+1) (natsF ()))) in
  natsF ()

let evens = map (fun x -> 2*x) nats
let odds = map (fun x -> x+1) evens



(* this one is cute *)

let ampl =
  let transf (v,(d,m)) =
    if d = 1 && v = m then (v-1,(-1,m))
    else if d = -1 && v = -m then (v+1,(1,m+1))
    else if d = 1 then (v+1,(1,m))
    else (v-1,(-1,m))  in
  let rec f () = fby (zip (cst 0) (cst (1,1)))
                     (fun () -> map transf (f ())) in
  map (fun (x,y) -> x) (f ())


(* streams of the form a0,a1,a2,a3... that are useful for
   illustrating question 3 *)

let tag tg =
  map (fun (t,n) -> t^(string_of_int n)) (zip (cst tg) nats)

let s_a = tag "a"
let s_b = tag "b"

let drop s = let (f,r) = split s in r

(* 
 * QUESTION 1 
 * 
 *)

let scale n s = map (fun (s) -> s*n) s

let mult s1 s2 = map (fun (x,y) -> x*y) (zip s1 s2)

let unzip s = ((map (fun (x,y) -> x) s), (map (fun (x,y) -> y) s))

let rec fold f init_s s = map (fun (i,sn) -> f sn i) (zip (fby init_s (fun () -> fold f init_s s)) s) (*i is growing into f (all the prev stuff) *)

let running_max s = fold (fun x y -> max x y) s s 

let rec stutter s = fby s 
  (fun () -> (fby s 
      (fun () -> (stutter (drop s)) )))



(*
 * QUESTION 2
 * 
 *)



let scalef n s = map (fun (s) -> s*.n) s

let addf s1 s2 = map (fun (x,y) -> x+.y) (zip s1 s2)

let minusf s1 s2 = map (fun (x,y) -> x-.y) (zip s1 s2)

let rec psumsf s =
  fby s (fun () -> addf (psumsf s) (drop s))  

let natsfl =
  let rec natsF () = fby (cst 0.0)
                         (fun () -> (map (fun x -> x+.1.0) (natsF ()))) in
  natsF ()

let evensf = map (fun x -> 2.0*.x) natsfl
let oddsf = map (fun x -> x+.1.0) evensf

let rec arctan z = psumsf (map (fun (n, d) -> ((-1.0)**((d-.1.0)/.2.0))*.((n**d)/.d) ) (zip (cst z) oddsf ))

(* PLACEHOLDER -- REPLACE WITH YOUR OWN DEFINITION *)

let pi = minusf (scalef 16.0 (arctan (1.0/.5.0))) (scalef 4.0 (arctan (1.0/.239.0)))
    
let rec newton f df guess = failwith "not" (*fold!!*)

let derivative f x = failwith "not implemented" (*x is x0*)

let limit epsilon s = failwith "not implemented"


(* 
 * QUESTION 3 
 * 
 *)

let first s = let (f,r) = split s in f
let rev_prefixes s = fold (fun x y -> x::y) (cst []) s

(* [["a0"]; ["a1"; "a0"]; ["a2"; "a1"; "a0"]; ["b3"; "b2"; "b1"; "b0"];
 ["a4"; "a3"; "a2"; "a1"; "a0"]; ["a5"; "a4"; "a3"; "a2"; "a1"; "a0"];
 ["a6"; "a5"; "a4"; "a3"; "a2"; "a1"; "a0"];
 ["a7"; "a6"; "a5"; "a4"; "a3"; "a2"; "a1"; "a0"];
 ["a8"; "a7"; "a6"; "a5"; "a4"; "a3"; "a2"; "a1"; "a0"];
 ["a9"; "a8"; "a7"; "a6"; "a5"; "a4"; "a3"; "a2"; "a1"; "a0"]] *)

let prefixes s = fold (fun s i -> i@[s]) (cst []) s

(* [["a0"]; ["a0"; "a1"]; ["a0"; "a1"; "a2"]; ["a0"; "a1"; "a2"; "a3"];
 ["a0"; "a1"; "a2"; "a3"; "a4"]; ["a0"; "a1"; "a2"; "a3"; "a4"; "a5"];
 ["a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"];
 ["a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7"];
 ["a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7"; "a8"];
 ["a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7"; "a8"; "a9"]] *)


(*From online, using zipL as a helper function
https://www.matt-mcdonnell.com/code/code_ocaml/ocaml_fold/ocaml_fold.html
*)
let rec zipL lst1 lst2 = match lst1,lst2 with
  | [],_ -> []
  | _, []-> []
  | (x::xs),(y::ys) -> (x,y) :: (zipL xs ys);;

let stripes s1 s2 = map (fun (l1, l2) -> zipL l1 l2) (zip (prefixes s1) (rev_prefixes s2))

(*  (a0,b0)  (a0,b1)  (a0,b2)  (a0,b3)  ...
 (a1,b0)  (a1,b1)  (a1,b2)  (a1,b3)  ...
 (a2,b0)  (a2,b1)  (a2,b2)  (a2,b3)  ...
 (a3,b0)  (a3,b1)  (a3,b2)  (a3,b3)  ...
   ...      ...      ...      ... 

-> for stripe 1
<- for stripe 2 *)

(*prefix 10 (filter (fun x y -> y>x) (cst 5) nats);;
- : int list = [6; 7; 8; 9; 10; 11; 12; 13; 14; 15]    *)
let popL lst s= 
  match lst with [] -> flatten (drop s)
  |h::t -> flatten (fby t (fun () -> (drop s)))

let rec flatten ss = map (fun ls -> popL ls ss ) ss

let pairs s1 s2 =  failwith "not implemented"
(*flatten stripes*)