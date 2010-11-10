%{ open Ast %}

%token COMMENT SEMICOLON LPAREN RPAREN LBRACE RBRACE COMMA MODULE INPUT OUTPUT
%token <string> ID
%token EOF

%start sourcecode
%type <Ast.sourcecode> sourcecode 

%%

sourcecode:
			{[]}
	| sourcecode moddecl { $2 :: $1 } 		

moddecl:
	MODULE ID LPAREN INPUT formals_opt SEMICOLON OUTPUT formals_opt RPAREN LBRACE RBRACE {{
		modname = $2;
		inputs = $5;
		outputs = $8;
		}}

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }
	
