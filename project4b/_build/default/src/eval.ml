open SmallCTypes
open EvalUtils
open TokenTypes


exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let rec eval_expr env t =
  match t with
	| Int i -> Int_Val i
	| Bool b -> Bool_Val b
	| ID s -> let rec helper v =
				(match v with
					| [] -> raise (DeclareError "ID not found")
					| h::e -> match h with (a,b) -> if a = s then b else helper e
				) in helper env
				
	| Add (e1, e2) -> let left = eval_expr env e1 in 
					  (match left with
					   | Int_Val l -> let right = eval_expr env e2 in
									(match right with
									 | Int_Val r -> Int_Val (l + r)
									 | _ -> raise (TypeError "Right value not valid for Add")
									)
					   | _ -> raise (TypeError "Left value not valid for Add")
					   )
					   
	| Sub (e1, e2) -> let left = eval_expr env e1 in 
					  (match left with
					   | Int_Val l -> let right = eval_expr env e2 in
									(match right with
									 | Int_Val r -> Int_Val (l - r)
									 | _ -> raise (TypeError "Right value not valid for Sub")
									)
					   | _ -> raise (TypeError "Left value not valid for Sub")
					   )
					   
	| Mult (e1, e2) -> let left = eval_expr env e1 in 
					  (match left with
					   | Int_Val l -> let right = eval_expr env e2 in
									(match right with
									 | Int_Val r -> Int_Val (l * r)
									 | _ -> raise (TypeError "Right value not valid for Mult")
									)
					   | _ -> raise (TypeError "Left value not valid for Mult")
					   )
					   
	| Div (e1, e2) -> let left = eval_expr env e1 in 
					  (match left with
					   | Int_Val l -> let right = eval_expr env e2 in
									(match right with
									 | Int_Val r -> if r = 0 then raise (DivByZeroError) else Int_Val (l/r)
									 | _ -> raise (TypeError "Right value not valid for Div")
									)
					   | _ -> raise (TypeError "Left value not valid for Div")
					   )
					   
	| Pow (e1, e2) -> let left = eval_expr env e1 in 
					  (match left with
					   | Int_Val l -> let right = eval_expr env e2 in
									(match right with
									 | Int_Val r -> if l = 1 then Int_Val 1 else if r < 0 then Int_Val 0 else let rec helper counter = if counter = r then 1 else l*(helper (counter + 1)) in Int_Val (helper 0)
									 | _ -> raise (TypeError "Right value not valid for Pow")
									)
					   | _ -> raise (TypeError "Left value not valid for Pow")
					   )
					   
	| Or (e1, e2) -> let left = eval_expr env e1 in
					 (match left with
					  | Bool_Val l -> let right = eval_expr env e2 in
									  (match right with
										| Bool_Val r -> Bool_Val (l || r)
										| _ -> raise (TypeError "Right value not valid for Or")
									  )
					  |	_ -> raise (TypeError "Left value not valid for Or")
					 )
					 
	| And (e1, e2) -> let left = eval_expr env e1 in
					  (match left with
						| Bool_Val l -> let right = eval_expr env e2 in
									  (match right with
										| Bool_Val r -> Bool_Val (l && r)
										| _ -> raise (TypeError "Right value not valid for And")
									  )
						| _ -> raise (TypeError "Left value not valid for And")
					 )
					 
	| Not e -> let v = eval_expr env e in
			   (match v with
				| Bool_Val b -> Bool_Val (not b)
				| _ -> raise (TypeError "Value not valid for Not")
			   )
			   
	| Greater (e1, e2) -> let left = eval_expr env e1 in
						  (match left with
							| Int_Val l -> let right = eval_expr env e2 in
										   (match right with
												| Int_Val r -> Bool_Val (l > r)
												| _ -> raise (TypeError "Right value not valid for Greater")
										    )
							| _ -> raise (TypeError "Left value not valid for Greater")
						  )
						  
	| Less (e1, e2) -> let left = eval_expr env e1 in
						  (match left with
							| Int_Val l -> let right = eval_expr env e2 in
										   (match right with
												| Int_Val r -> Bool_Val (l < r)
												| _ -> raise (TypeError "Right value not valid for Less")
										    )
							| _ -> raise (TypeError "Left value not valid for Less")
						  )
						  
	| GreaterEqual (e1, e2) -> let left = eval_expr env e1 in
						  (match left with
							| Int_Val l -> let right = eval_expr env e2 in
										   (match right with
												| Int_Val r -> Bool_Val (l >= r)
												| _ -> raise (TypeError "Right value not valid for GreaterEqual")
										    )
							| _ -> raise (TypeError "Left value not valid for GreaterEqual")
						  )
						  
	| LessEqual (e1, e2) -> let left = eval_expr env e1 in
						  (match left with
							| Int_Val l -> let right = eval_expr env e2 in
										   (match right with
												| Int_Val r -> Bool_Val (l <= r)
												| _ -> raise (TypeError "Right value not valid for LessEqual")
										    )
							| _ -> raise (TypeError "Left value not valid for LessEqual")
						  )
						  
	| Equal (e1, e2) -> let left = eval_expr env e1 in
						(match left with
						  | Int_Val l -> let right = eval_expr env e2 in
										 (match right with
											| Int_Val r -> Bool_Val (l = r)
											| _ -> raise (TypeError "Left and Right values don't equal in Equal (L = Int, R = Bool)")
										 )
						  | Bool_Val l -> let right = eval_expr env e2 in
										 (match right with
											| Bool_Val r -> Bool_Val (l = r)
											| _ -> raise (TypeError "Left and Right values don't equal in Equal (L = Bool, R = Int)")
										 )
						)
										 
	| NotEqual (e1, e2) -> let left = eval_expr env e1 in
						(match left with
						  | Int_Val l -> let right = eval_expr env e2 in
										 (match right with
											| Int_Val r -> Bool_Val (l <> r)
											| _ -> raise (TypeError "Left and Right values don't equal in NotEqual (L = Int, R = Bool)")
										 )
						  | Bool_Val l -> let right = eval_expr env e2 in
										 (match right with
											| Bool_Val r -> Bool_Val (l <> r)
											| _ -> raise (TypeError "Left and Right values don't equal in NotEqual (L = Bool, R = Int)")
										 )
						)
						
let rec contains_env lst v =
	match lst with
		[] -> false
		| h::t -> match h with (a, b) -> if a = v then true else contains_env t v

let rec eval_stmt env s =
	match s with
	 | NoOp -> env
	 | Seq (s1, s2) -> let env1 = eval_stmt env s1 in
					   eval_stmt env1 s2
	 | Declare (typ, str) -> if contains_env env str then raise (DeclareError "Var already exists.") else
							(match typ with
							  | Int_Type -> env@[(str, Int_Val 0)]
							  | Bool_Type -> env@[(str, Bool_Val false)]
							)
								
	 | Assign (str, e) -> if (contains_env env str) = false then raise (DeclareError "Var doesn't exist.") else
						  let rec new_env lst =
							(match lst with
							 | [] -> []
							 | h::t -> match h with (a, b) -> if a <> str then [h]@(new_env t) else let e1 = eval_expr env e in 
															  (match e1 with
																| Int_Val i -> (match b with Int_Val j -> [(str, e1)]@t | _ -> raise (TypeError "Invalid assignment."))
																| Bool_Val bo -> (match b with Bool_Val j -> [(str, e1)]@t | _ -> raise (TypeError "Invalid assignment."))
															   )
							 ) in new_env env
	 | If (e, s1, s2) -> let expr = eval_expr env e in
						 (match expr with
						   | Bool_Val b -> if b then eval_stmt env s1 else eval_stmt env s2
						   | _ -> raise (TypeError "If statement doesn't work with Ints")
						  )
	 | While (e, s) -> let rec loop lst =
					   let exp = eval_expr lst e in
					   (match exp with
						 | Bool_Val b -> if b then loop (eval_stmt lst s) else lst
						 | _ -> raise (TypeError "While statement doesn't work with Ints")
					   ) in loop env
	 | For (str, e1, e2, s) -> let left = eval_expr env e1 in
							   (match left with
							     | Int_Val l -> let right = eval_expr env e2 in
								                (match right with
												  | Int_Val r -> let rec loop lst counter =
																 (match lst with
																	| [] -> raise (TypeError "This literally shouldn't happen")
																	| h::t -> if counter > r then [(str, Int_Val counter)]@t else 
																		    let env1 = eval_stmt lst s in
																			(match env1 with
																			 | [] -> raise (TypeError "This literally shouldn't happen")
																			 | h::t -> match h with (a, b) -> (match b with 
																												| Int_Val i -> loop ([(str, Int_Val (i + 1))]@t) (i + 1)
																												| Bool_Val b -> raise (TypeError "This literally shouldn't happen")
																											   )
																			 )
																 ) in loop ([(str, Int_Val l)]@env) l
												  | _ -> raise (TypeError "Right isn't Int in For")
												 )
								 | _ -> raise (TypeError "Left isn't Int in For")
								)
							   
	 | Print e -> let expr = eval_expr env e in
				  (match expr with
					| Int_Val i -> let _ = print_output_int i in let _ = print_output_newline() in env
					| Bool_Val b -> let _ = print_output_bool b in let _ = print_output_newline() in env
				  )