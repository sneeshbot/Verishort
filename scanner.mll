{ open Parser }

rule token = parse 
	[' ' '\t' '\r' '\n'] 	{ token lexbuf }	 			(*Whitespace*)
	| "/*"     { comment lexbuf }           (* Comments *)
    | "//"     { comment2 lexbuf }
	| '(' { LPAREN }	
    | ')' { RPAREN }				(*Punctuation*)
	| '{' { LBRACE }	
    | '}' { RBRACE }
	| '[' { LBRACKET }
	| ']' { RBRACKET }
	| ';' { SEMICOLON } 
    | ',' { COMMA }
	| ':' { COLON }
	| ['0' - '9']+ 'd' as var { DLIT( int_of_string (String.sub var 0 (String.length var - 1) ) ) } (* Literals *)
	| ['0' '1']+ 'b' as var { BLIT (String.sub var 0 (String.length var - 1) )  }
	| ['0' '1' 'x']+ 'b' as var { XLIT(String.sub var 0 (String.length var - 1)) } 
	| ['0' - '9']+ as var { DECLIT( int_of_string var ) }
	| '+'  { PLUS }  (* Operators *) 
	| '-'  { MINUS }
	| '*'  { MULTIPLY }
	| '/'  { DIVIDE }
	| '%'  { MODULUS }
	| "<<" { LSHIFT }
	| ">>" { RSHIFT }
	| '\'' { SIGEXT }
	| "!&" { NAND }
	| "!|" { NOR }
	| "!^" { XNOR }
	| '!'  { NOT }
	| '&'  { AND }
	| '|'  { OR }
	| '^'  { XOR }
	| "==" { EQ }
	| "!=" { NE }
	| "<=" { LE }
	| ">=" { GE }
	| '<'  { LT }
	| '>'  { GT }
	| '='  { ASSIGN }
	| "case"      { CASE }  (*Keywords*)
	| "clock"     { CLOCK }
	| "concat"    { CONCAT }
	| "else"      { ELSE }
	| "for"       { FOR }
	| "if"        { IF }
	| "input"     { INPUT }
	| "module"    { MODULE }
	| "negedge"	  { NEGEDGE }							
    | "output"    { OUTPUT }
	| "parameter" { PARAMETER }
	| "posedge"   { POSEDGE }
	| "register"  { REG }
	| "return"    { RETURN }
	| "reset"     { RESET }
	| "wire"      { WIRE }
	| eof         { EOF }										(*EOF*)
	| ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as var { ID(var) }
	| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and comment2 = parse
  "\n" { token lexbuf }
| _ {comment lexbuf}
