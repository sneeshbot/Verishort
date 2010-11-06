{ open Parser}

rule token = parse 
	[' ' '\t' '\r' '\n'] 	{token lexbuf}	 			(*Whitespace*)
	| "/*"_*"*/"|"//"_*'\n' { COMMENT }					(*Comments.  These are not the same as example comments in microC because we want to translate this thorough instead of discarding*)
	| '(' { LPAREN }	| ')' { RPAREN }				(*Punctuation*)
	| '{' { LBRACE }	| '}' { RBRACE }
	| ';' { SEMICOLON } | ',' { COMMA }
	| "module" { MODULE}								(*Keywords*)
	| "input" {INPUT}		| "output" {OUTPUT}
	| eof { EOF }										(*EOF*)
	| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as var { VAR(var) }
	| _ as char { raise (FAILURE("illegal character " ^ Char.escaped char)) }
