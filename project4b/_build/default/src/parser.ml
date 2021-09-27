open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(* Parsing (TODO: implement your code below) *)

let rec parse_expr toks : expr_result =   
	parse_or_expr toks
	
and parse_or_expr toks = 
	let (t, exp) = parse_and_expr toks in
		match lookahead t with
			| Tok_Or -> let t2 = match_token t Tok_Or in 
				let (t3, exp2) = parse_or_expr t2 in
					(t3, Or(exp, exp2))
			| _ -> (t, exp)

and parse_and_expr toks =
	let (t, exp) = parse_equality_expr toks in 
		match lookahead t with
			| Tok_And -> let t2 = match_token t Tok_And in
				let (t3, exp2) = parse_and_expr t2 in
					(t3, And(exp, exp2))
			| _ -> (t, exp)

and parse_equality_expr toks =
	let (t, exp) = parse_relational_expr toks in
		match lookahead t with
			| Tok_Equal -> let t2 = match_token t Tok_Equal in
				let (t3, exp2) = parse_equality_expr t2 in
					(t3, Equal(exp, exp2))
			| Tok_NotEqual -> let t2 = match_token t Tok_NotEqual in
				let (t3, exp2) = parse_equality_expr t2 in
					(t3, NotEqual(exp, exp2))
			| _ -> (t, exp)

and parse_relational_expr toks =
	let (t, exp) = parse_additional_expr toks in
		match lookahead t with
			| Tok_Greater -> let t2 = match_token t Tok_Greater in
				let (t3, exp2) = parse_relational_expr t2 in
					(t3, Greater(exp, exp2))
			| Tok_Less -> let t2 = match_token t Tok_Less in
				let (t3, exp2) = parse_relational_expr t2 in
					(t3, Less(exp, exp2))
			| Tok_GreaterEqual -> let t2 = match_token t Tok_GreaterEqual in
				let (t3, exp2) = parse_relational_expr t2 in
					(t3, GreaterEqual(exp, exp2))
			| Tok_LessEqual -> let t2 = match_token t Tok_LessEqual in
				let (t3, exp2) = parse_relational_expr t2 in
					(t3, LessEqual(exp, exp2))
			| _ -> (t, exp)

and parse_additional_expr toks = 
	let (t, exp) = parse_multiplicative_expr toks in
		match lookahead t with
			| Tok_Add -> let t2 = match_token t Tok_Add in
				let (t3, exp2) = parse_additional_expr t2 in
					(t3, Add(exp, exp2))
			| Tok_Sub -> let t2 = match_token t Tok_Sub in
				let (t3, exp2) = parse_additional_expr t2 in
					(t3, Sub(exp, exp2))
			| _ -> (t, exp)

and parse_multiplicative_expr toks = 
	let (t, exp) = parse_power_expr toks in
		match lookahead t with
			| Tok_Mult -> let t2 = match_token t Tok_Mult in
				let (t3, exp2) = parse_multiplicative_expr t2 in
					(t3, Mult(exp, exp2))
			| Tok_Div -> let t2 = match_token t Tok_Div in
				let (t3, exp2) = parse_multiplicative_expr t2 in
					(t3, Div(exp, exp2))
			| _ -> (t, exp)

and parse_power_expr toks = 
	let (t, exp) = parse_unary_expr toks in
		match lookahead t with
			| Tok_Pow -> let t2 = match_token t Tok_Pow in
				let (t3, exp2) = parse_power_expr t2 in
					(t3, Pow(exp, exp2))
			| _ -> (t, exp)

and parse_unary_expr toks = 
	match lookahead toks with
		| Tok_Not -> let t2 = match_token toks Tok_Not in
			let (t3, exp) = parse_unary_expr t2 in
				(t3, Not(exp))
		| _ -> parse_primary_expr toks

and parse_primary_expr toks = 
	match lookahead toks with
		| Tok_Int n -> let t2 = match_token toks (Tok_Int n) in 
			(t2, Int n)
		| Tok_Bool b -> let t2 = match_token toks (Tok_Bool b) in
			(t2, Bool b)
		| Tok_ID s -> let t2 = match_token toks (Tok_ID s) in
			(t2, ID s)
		| Tok_LParen -> let t2 = match_token toks (Tok_LParen) in 
			let (t3, exp) = parse_expr t2 in 
				(match lookahead t3 with
					| Tok_RParen -> let t4 = match_token t3 Tok_RParen in 
						(t4, exp)
					| _ -> raise (InvalidInputException "Tok_RParen is missing for expr")
				)
		| _ -> raise (InvalidInputException "Bad Token")
		
let rec parse_stmt toks : stmt_result = 
	let (t, stmt) = stmt_option toks in
	match stmt with
		| NoOp -> raise (InvalidInputException "NoOp was returned")
		| _ -> (t, stmt)
	
and stmt_option toks =
	match lookahead toks with 
		| Tok_Int_Type -> let (t2, stmt) = parse_dec_stmt toks in
			let (t3, stmt2) = stmt_option t2 in
				(t3, Seq(stmt, stmt2))
		| Tok_Bool_Type -> let (t2, stmt) = parse_dec_stmt toks in
			let (t3, stmt2) = stmt_option t2 in
				(t3, Seq(stmt, stmt2))
		| Tok_ID s -> let (t2, stmt) = parse_assign_stmt toks in
			let (t3, stmt2) = stmt_option t2 in
				(t3, Seq(stmt, stmt2))
		| Tok_Print -> let (t2, stmt) = parse_print_stmt toks in
			let (t3, stmt2) = stmt_option t2 in
				(t3, Seq(stmt, stmt2))
		| Tok_If -> let (t2, stmt) = parse_if_stmt toks in
			let (t3, stmt2) = stmt_option t2 in
				(t3, Seq(stmt, stmt2))
		| Tok_For -> let (t2, stmt) = parse_for_stmt toks in
			let (t3, stmt2) = stmt_option t2 in
				(t3, Seq(stmt, stmt2))
		| Tok_While -> let (t2, stmt) = parse_while_stmt toks in
			let (t3, stmt2) = stmt_option t2 in
				(t3, Seq(stmt, stmt2))
		| _ -> (toks, NoOp)
		
and parse_dec_stmt toks =
	match lookahead toks with
		| Tok_Int_Type -> let t2 = match_token toks Tok_Int_Type in
				(match lookahead t2 with
					| Tok_ID s -> let t3 = match_token t2 (Tok_ID s) in
								  let t4 = match_token t3 Tok_Semi in (t4, Declare(Int_Type, s))
					| _ -> raise (InvalidInputException "Not ID in Int Dec")
				)
				
		| Tok_Bool_Type -> let t2 = match_token toks Tok_Bool_Type in
				(match lookahead t2 with
					| Tok_ID s -> let t3 = match_token t2 (Tok_ID s) in
								  let t4 = match_token t3 Tok_Semi in (t4, Declare(Bool_Type, s))
					| _ -> raise (InvalidInputException "Not ID in Bool Dec")
				)
				
		| _ -> raise (InvalidInputException "Bad Dec")

and parse_assign_stmt toks =
	match lookahead toks with
		| Tok_ID s -> let t2 = match_token toks (Tok_ID s) in
					  let t3 = match_token t2 Tok_Assign in
					  let (t4, expr) = parse_expr t3 in
					  let t5 = match_token t4 Tok_Semi in (t5, Assign(s, expr))
		| _ -> raise (InvalidInputException "Bad Assign")

and parse_print_stmt toks =
	match lookahead toks with
		| Tok_Print -> let t2 = match_token toks Tok_Print in
					   let t3 = match_token t2 Tok_LParen in
					   let (t4, expr) = parse_expr t3 in
					   let t5 = match_token t4 Tok_RParen in
					   let t6 = match_token t5 Tok_Semi in (t6, Print(expr))
		| _ -> raise (InvalidInputException "Bad Print")

and parse_if_stmt toks = 
	match lookahead toks with
		| Tok_If -> let t2 = match_token toks Tok_If in
					let t3 = match_token t2 Tok_LParen in
					let (t4, expr) = parse_expr t3 in
					let t5 = match_token t4 Tok_RParen in
					let t6 = match_token t5 Tok_LBrace in
					let (t7, stmt) = stmt_option t6 in
					let t8 = match_token t7 Tok_RBrace in
					(match lookahead t8 with
						| Tok_Else -> let t9 = match_token t8 Tok_Else in
									  let t10 = match_token t9 Tok_LBrace in
									  let (t11, stmt2) = stmt_option t10 in
							          let t12 = match_token t11 Tok_RBrace in (t12, If(expr, stmt, stmt2))
						| _ -> (t8, If(expr, stmt, NoOp))
					)
		| _ -> raise (InvalidInputException "Bad If")

and parse_for_stmt toks =
	match lookahead toks with
		| Tok_For -> let t2 = match_token toks Tok_For in
					 let t3 = match_token t2 Tok_LParen in
					 (match lookahead t3 with
						| Tok_ID s -> let t4 = match_token t3 (Tok_ID s) in
									  let t5 = match_token t4 Tok_From in
									  let (t6, expr) = parse_expr t5 in
					                  let t7 = match_token t6 Tok_To in
					                  let (t8, expr2) = parse_expr t7 in
					                  let t9 = match_token t8 Tok_RParen in
					                  let t10 = match_token t9 Tok_LBrace in
					                  let (t11, stmt) = stmt_option t10 in
				                      let t12 = match_token t11 Tok_RBrace in (t12, For(s, expr, expr2, stmt))
						| _ -> raise (InvalidInputException "Tok_ID not in For")
					 )
		| _ -> raise (InvalidInputException "Bad For")

and parse_while_stmt toks =
	match lookahead toks with
		| Tok_While -> let t2 = match_token toks Tok_While in
					   let t3 = match_token t2 Tok_LParen in
					   let (t4, expr) = parse_expr t3 in
					   let t5 = match_token t4 Tok_RParen in
					   let t6 = match_token t5 Tok_LBrace in
					   let (t7, stmt) = stmt_option t6 in
					   let t8 = match_token t7 Tok_RBrace in (t8, While(expr, stmt))
		| _ -> raise (InvalidInputException "Bad While")

let parse_main toks : stmt = 
	match lookahead toks with
		| Tok_Int_Type -> let t2 = match_token toks Tok_Int_Type in
						  let t3 = match_token t2 Tok_Main in
					      let t4 = match_token t3 Tok_LParen in
						  let t5 = match_token t4 Tok_RParen in
						  let t6 = match_token t5 Tok_LBrace in
						  let (t7, stmt) = stmt_option t6 in
						  let t8 = match_token t7 Tok_RBrace in
						  if t8 <> [EOF] then raise (InvalidInputException "Main doesn't end with EOF") else stmt
		| _ -> raise (InvalidInputException "Not an Int_Type in Main")
