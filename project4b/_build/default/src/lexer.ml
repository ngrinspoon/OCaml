open TokenTypes

let tokenize input =
  let length = String.length input in
	let rec helper pos = 
		if pos >= length then [EOF]
		else if (Str.string_match (Str.regexp "-?[0-9]+") input pos) then
			let matched_int = Str.matched_string input in
			Tok_Int (int_of_string matched_int)::(helper (pos + (String.length matched_int)))
		else if (Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos) then
			let matched_str = Str.matched_string input in
			if matched_str = "int" then Tok_Int_Type::(helper (pos + 3))
			else if matched_str = "bool" then Tok_Bool_Type::(helper (pos + 4))
			else if matched_str = "true" then Tok_Bool (true)::(helper (pos + 4))
			else if matched_str = "false" then Tok_Bool (false)::(helper (pos + 5))
			else if matched_str = "printf" then Tok_Print::(helper (pos + 6))
			else if matched_str = "main" then Tok_Main::(helper (pos + 4))
			else if matched_str = "if" then Tok_If::(helper (pos + 2))
			else if matched_str = "else" then Tok_Else::(helper (pos + 4))
			else if matched_str = "for" then Tok_For::(helper (pos + 3))
			else if matched_str = "from" then Tok_From::(helper (pos + 4))
			else if matched_str = "to" then Tok_To::(helper (pos + 2))
			else if matched_str = "while" then Tok_While::(helper (pos + 5))
			else Tok_ID (matched_str)::(helper (pos + (String.length matched_str)))
		else if (Str.string_match (Str.regexp "(") input pos) then
			Tok_LParen::(helper (pos + 1))
		else if (Str.string_match (Str.regexp ")") input pos) then
			Tok_RParen::(helper (pos + 1))
		else if (Str.string_match (Str.regexp "{") input pos) then
			Tok_LBrace::(helper (pos + 1))
		else if (Str.string_match (Str.regexp "}") input pos) then
			Tok_RBrace::(helper (pos + 1))
		else if (Str.string_match (Str.regexp "==") input pos) then
			Tok_Equal::(helper (pos + 2))
		else if (Str.string_match (Str.regexp "!=") input pos) then
			Tok_NotEqual::(helper (pos + 2))
		else if (Str.string_match (Str.regexp ">=") input pos) then
			Tok_GreaterEqual::(helper (pos + 2))
		else if(Str.string_match (Str.regexp "<=") input pos) then
			Tok_LessEqual::(helper (pos + 2))
		else if (Str.string_match (Str.regexp "=") input pos) then
			Tok_Assign::(helper (pos + 1))
		else if (Str.string_match (Str.regexp ">") input pos) then
			Tok_Greater::(helper (pos + 1))
		else if (Str.string_match (Str.regexp "<") input pos) then
			Tok_Less::(helper (pos + 1))
		else if (Str.string_match (Str.regexp "||") input pos) then
			Tok_Or::(helper (pos + 2))
		else if (Str.string_match (Str.regexp "&&") input pos) then
			Tok_And::(helper (pos + 2))
		else if (Str.string_match (Str.regexp "!") input pos) then
			Tok_Not::(helper (pos + 1))
		else if (Str.string_match (Str.regexp ";") input pos) then
			Tok_Semi::(helper (pos + 1))
		else if (Str.string_match (Str.regexp "\\+") input pos) then
			Tok_Add::(helper (pos + 1))
		else if (Str.string_match (Str.regexp "-") input pos) then
			Tok_Sub::(helper (pos + 1))
		else if (Str.string_match (Str.regexp "\\*") input pos) then
			Tok_Mult::(helper (pos + 1))
		else if (Str.string_match (Str.regexp "/") input pos) then
			Tok_Div::(helper (pos + 1))
		else if (Str.string_match (Str.regexp "\\^") input pos) then
			Tok_Pow::(helper (pos + 1))
		else helper (pos + 1)
	in helper 0

