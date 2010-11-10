{ open Parser}

rule token = parse 
	[' ' '\t' '\r' '\n'] 	{token lexbuf}	 			(*Whitespace*)
	| "/*"     { comment lexbuf }           (* Comments *)
| "//"     { comment2 lexbuf }
	| '(' { LPAREN }	
    | ')' { RPAREN }				(*Punctuation*)
	| '{' { LBRACE }	
    | '}' { RBRACE }
	| ';' { SEMICOLON } 
    | ',' { COMMA }
	| "module" { MODULE}								(*Keywords*)
	| "input" {INPUT}		
    | "output" {OUTPUT}
	| eof { EOF }										(*EOF*)
	| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_' '$']* as var { ID(var) }
	| _ as char { raise (FAILURE("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and comment2 = parse
  "\n" { token lexbuf }
| _ {comment lexbuf}
