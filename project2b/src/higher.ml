open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let count_occ lst target = fold(fun count head -> if head = target then count + 1 else count) 0 lst
	
let uniq lst = fold(fun l head -> if (count_occ l head = 1) then l else l@[head]) [] lst

let assoc_list lst = 
	let lst2 = uniq lst in
		map (fun head -> (head, count_occ lst head)) lst2

let ap fns args = fold(fun lst head -> lst@(map head args)) [] fns
