{ open Parser 

let incr_linenum lexbuf =
let pos = lexbuf.Lexing.lex_curr_p in
lexbuf.Lexing.lex_curr_p <- { pos with
	Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
	Lexing.pos_bol = pos.Lexing.pos_cnum;
}



}



rule token = parse 
	[' ' '\t' '\r' ] 	{ token lexbuf }	 			(*Whitespace*)
	| '\n'  { incr_linenum lexbuf;token lexbuf }
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
	| ['0' - '9']+  as var { DLIT( int_of_string var )  } (* Literals *)
	| ['0' '1']+ 'b' as var { BLIT (String.sub var 0 (String.length var - 1) )  }
	| ['0' '1' 'x']+ 'b' as var { XLIT(String.sub var 0 (String.length var - 1)) } 
	| '+'  { PLUS }  (* Operators *) 
	| '-'  { MINUS }
	| '*'  { MULTIPLY }
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
	| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as var { ID(var) }
	| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| '\n' { incr_linenum lexbuf; comment lexbuf}
| _    { comment lexbuf }

and comment2 = parse
  '\n' { incr_linenum lexbuf; token lexbuf }
| _ {comment2 lexbuf}
