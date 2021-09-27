(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = 
	match tup with
		(x, y, z) -> (z, y, x);;

let abs x = if x < 0 then x * -1 else x;;

let area x y = 
	match x with
		(a, b) -> match y with
			(c, d) -> abs ((c-a)*(d-b))
;;

let volume x y = 
	match x with
		(a, b, c) -> match y with
			(d, e, f) -> abs ((d-a)*(e-b)*(f-c))
;;

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec factorial x = 
	match x with
	0 -> 1
	| 1 -> 1
	| n -> n * factorial (x-1)
;;

let rec pow x y = 
	match y with
	0 -> 1
	| n -> x * (pow x (n - 1))
;;

let rec log x y = if y < x then 0 else 1 + (log x (y/x));;
		
let rec is_prime_aux x y = 
	match y with
	1 -> true
	| n -> if x mod n != 0 then is_prime_aux x (n-1) else false
;;
	
let is_prime x = if x <= 1 || x > 2 && x mod 2 == 0 then false else is_prime_aux x (x-1);;

let rec next_prime x = if is_prime x then x else next_prime (x+1);;

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst = 
	match lst with
	[] -> failwith "Out of bounds"
	| a::t -> if idx == 0 then a else get (idx-1) t
;;

let rec length lst = 
	match lst with
	[] -> 0
	| a::t -> 1 + length t
;;

let larger lst1 lst2 = 
	let x = length lst1 in
	let y = length lst2 in
	if x == y then []
	else if x < y then lst2
	else lst1
;;

let rec append l m = 
	match l with
	[] -> m
	| (x::xs) -> x::(append xs m)
;;

let rec reverse lst = 
	match lst with
	[] -> []
	| (x::xs) -> append (reverse xs) [x]
;;

let rec combine lst1 lst2 = 
	match lst1 with
	[] -> lst2
	| h::t -> h::(combine t lst2)
;;

let rec rotate shift lst = 
	match lst with
	[] -> []
	| h::t -> if shift == 0 then lst else rotate (shift-1) (combine t [h])
;;
