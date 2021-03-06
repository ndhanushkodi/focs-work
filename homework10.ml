(* 

HOMEWORK 10

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


type 'a bintree =
  | Empty
  | Node of 'a * 'a bintree * 'a bintree


let sample = Node(10,Node(3,Node(7,Empty,Empty),
                            Node(5,Empty,Empty)),
                     Node(6,Node(99,Empty,
                                 Node(66,Empty,Empty)),
                          Empty))


(* Printing an integer binary tree *)

let pbt bt =
  let rec loop bt depth = 
    match bt with
    | Empty -> ()
    | Node(n,left,right) ->
	(loop right (depth^"    ");
         print_endline (depth^(string_of_int n));
         loop left (depth^"    ")) in
  loop bt ""





(* Q1 *)

let rec size t = 
  match t with
  | Empty -> 0
  | Node (n, lt, rt) -> 1+ (size lt) + (size rt)


let rec sum t = 
  match t with
  | Empty -> 0
  | Node (n, lt, rt) -> n + (sum lt) + (sum rt)


let rec height t = 
  match t with
  | Empty -> 0
  | Node (n, lt, rt) -> 1 + max (height lt) (height rt)

let isEmpty t = 
  match t with 
  | Empty -> true
  | Node (n, lt, rt) -> false

let rec fringe t = 
  match t with 
  | Empty -> []
  | Node (n, lt, rt) -> if ((isEmpty lt)&&(isEmpty rt)) then ([n]@(fringe lt))@(fringe rt) else (fringe lt)@(fringe rt) 


let rec map f t = 
  match t with 
  | Empty -> Empty
  | Node (n, lt, rt) -> Node((f n), (map f lt), (map f rt))


let rec fold f t b = 
  match t with
  | Empty -> b
  | Node (n, lt, rt) -> f n (fold f lt b) (fold f rt b)


let preorder t = fold (fun root left right -> ([root]@left)@right ) t []


let postorder t = fold (fun root left right -> (left@right)@[root] ) t []


let inorder t = fold (fun root left right -> (left@[root])@right ) t []


let rec bst_insert t x = 
  match t with 
  | Empty -> Node(x, Empty, Empty)
  | Node (n, lt, rt) -> if x<n then (Node (n, (bst_insert lt x), rt)) else (Node (n,lt,(bst_insert rt x)))



let rec bst_lookup t x = 
  match t with
  | Empty -> false
  | Node (n, lt, rt) -> if x=n then true else (if x<n then (bst_lookup lt x) else (bst_lookup rt x))


let rec bstify t = 
  let list_nodes = preorder t in
    List.fold_right (fun n acc -> bst_insert acc n) list_nodes Empty
    

(*   | Empty -> Empty
  | Node (n, Node(ln, ll, lr), rt) ->  bst_insert (bstify lt) n
  | Node (n, Empty, rt) *)
  (*fold (fun root left right -> bst_insert (bst_insert root left) right ) t (bst_insert Empty n)*)

let avl_insert t x = failwith ("avl_insert not implemented")


(* Node n bstify lt bstify rt  *)