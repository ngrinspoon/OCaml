open Funs

(************************)
(* Part 2: Integer BSTs *)
(************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int_tree * int_tree

let empty_int_tree = IntLeaf

let rec int_insert x t =
  match t with
  | IntLeaf -> IntNode(x, IntLeaf, IntLeaf)
  | IntNode (y, l, r) when x > y -> IntNode (y, l, int_insert x r)
  | IntNode (y, l, r) when x = y -> t
  | IntNode (y, l, r) -> IntNode (y, int_insert x l, r)

let rec int_mem x t =
  match t with
  | IntLeaf -> false
  | IntNode (y, l, r) when x > y -> int_mem x r
  | IntNode (y, l, r) when x = y -> true
  | IntNode (y, l, r) -> int_mem x l

(* Implement the functions below. *)

let rec int_size t = match t with
	IntLeaf -> 0
	| IntNode (x, y, z) -> 1 + int_size (y) + int_size(z)

let rec int_max_aux m t =
	match t with
		IntLeaf -> m
		| IntNode (x, y, z) -> int_max_aux x z
		
let rec int_max t = match t with
	IntLeaf -> invalid_arg("int_max")
	| IntNode(x, y, z) -> int_max_aux x z

(****************************)
(* Part 3: Polymorphic BSTs *)
(****************************)

type 'a atree =
  | Leaf
  | Node of 'a * 'a atree * 'a atree
type 'a compfn = 'a -> 'a -> int
type 'a ptree = 'a compfn * 'a atree

let empty_ptree f : 'a ptree = (f,Leaf)

(* Implement the functions below. *)

let rec ainsert x fn t = match t with
	Leaf -> Node (x, Leaf, Leaf)
	| Node (v, l, r) when (fn x v) < 0 -> Node(v, ainsert x fn l, r)
	| Node (v, l, r) when (fn x v) = 0 -> t
	| Node (v, l, r) -> Node(v, l, ainsert x fn r)

let pinsert x t = match t with
	(fn, tree) -> (fn, ainsert x fn tree)
	
let rec amem x fn t = match t with
	Leaf -> false
	| Node (v, l, r) when (fn x v) < 0 -> amem x fn l
	| Node (v, l, r) when (fn x v) = 0 -> true
	| Node (v, l, r) -> amem x fn r

let pmem x t = match t with
	(fn, tree) -> amem x fn tree

let pinsert_all lst t = fold(fun ptree head -> pinsert head ptree) t lst

let rec a_as_list t = match t with
	Leaf -> []
	| Node (v, l, r) -> (a_as_list l)@[v]@(a_as_list r)
	
let rec p_as_list t = match t with
	(fn, tree) -> a_as_list tree

let pmap f t = let lst = p_as_list t in let mp = map f lst in match t with
	(fn, tree) -> pinsert_all mp (empty_ptree fn)

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)
type lookup_table = 
	| Empty
	| Table of (string * int) list * lookup_table

let empty_table () : lookup_table = Empty

let push_scope (table: lookup_table) : lookup_table = match table with
	Empty -> Table ([], table)
	| Table (lst, tbl) -> Table ([], table) 

let pop_scope (table: lookup_table) : lookup_table = match table with
	Empty -> failwith "No scopes remain!"
	| Table (lst, tbl) -> tbl

let add_var name value (table: lookup_table) : lookup_table = match table with
	Empty -> failwith "There are no scopes to add a variable to!"
	| Table (lst, tbl) -> Table ([(name, value)]@lst, tbl)
	
let rec lookup name (table: lookup_table) = match table with
	Empty -> failwith "Variable not found!"
	| Table (lst, tbl) -> let (y, boo) = fold (fun (cur, is_found) head -> if is_found then (cur, true) else match head with (x, y) -> if name = x then (y, true) else (cur, false)) (0, false) lst in if boo then y else lookup name tbl 

(*******************************)
(* Part 5: Shapes with Records *)
(*******************************)

type pt = { x: int; y: int };;
type shape =
  | Circ of { radius: float; center: pt }
  | Square of { length: float; upper: pt }
  | Rect of { width: float; height: float; upper: pt }
;;

(* Implement the functions below. *)

let area s = match s with
	Circ {radius=r; center=c} -> r *. r *. 3.14
	| Rect {width=w; height=h; upper=u} -> w *. h
	| Square {length=l; upper=u} -> l *. l

let rec filter f lst = match lst with
	[] -> []
	| (h::t) -> if f h then h::(filter f t) else filter f t
