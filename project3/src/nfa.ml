open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let count_occ lst target = List.fold_left (fun count head -> if head = target then count + 1 else count) 0 lst

let uniq lst = List.fold_left (fun l head -> if (count_occ l head = 1) then l else l@[head]) [] lst

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  let rec delta_move lst opt = match lst with
	[] -> []
	|h::t -> match h with
		(a, b, c) -> if a = opt && s = b then c::(delta_move t opt) else delta_move t opt
	in let l = List.fold_left (fun x head -> x@(delta_move nfa.delta head)) [] qs in uniq l
	
let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  let rec delta_closure lst opt ret_lst = match lst with
	[] -> ret_lst
	|h::t -> match h with
		(a, b, c) -> let p = delta_closure t opt ret_lst in if a = opt && b = None && (List.mem c ret_lst) = false then p@(delta_closure nfa.delta c (ret_lst@[c])) else p
	in let l = List.fold_left (fun x head -> x@(delta_closure nfa.delta head [head])) [] qs in uniq l

let rec any_subset a b = match a with
	[] -> false
	|f::e -> if List.mem f b then true else any_subset e b
		
let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  let letters = explode s in
	let rec search closures ltrs = match ltrs with
		[] -> if closures = [] then false else any_subset closures nfa.fs
		|h::t -> let moves = move nfa closures (Some h) in search (e_closure nfa moves) t
	in search (e_closure nfa [nfa.q0]) letters
			

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  List.map (fun h -> let moves = move nfa qs (Some h) in e_closure nfa moves) nfa.sigma

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  List.map (fun h -> let moves = move nfa qs (Some h) in let closures = e_closure nfa moves in (qs, (Some h), closures)) nfa.sigma

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = if any_subset qs nfa.fs then [qs] else []

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
		match work with
		[] -> dfa
		|h::t -> if List.mem h dfa.qs then nfa_to_dfa_step nfa dfa t else
			let n_states = new_states nfa h in
			let n_finals = new_finals nfa h in
			let n_trans = new_trans nfa h in
			nfa_to_dfa_step nfa ({sigma = dfa.sigma; qs = dfa.qs@[h]; q0 = dfa.q0; fs = dfa.fs@n_finals; delta = dfa.delta @ n_trans}) (t@n_states) 

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let r0 = (e_closure nfa [nfa.q0]) in 
	let w = [r0] in
		nfa_to_dfa_step nfa {sigma = nfa.sigma; qs = []; q0 = r0; fs = []; delta = []} w
