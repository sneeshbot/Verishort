%{ open Ast %}

%token COMMENT SEMICOLON LPAREN RPAREN LBRACE RBRACE

%start sourcecode
%type <Ast.top> sourcecode 

%%

sourcecode:
			{[],[]}
	| sourcecode moddecl { ($2 		

(*moddecl:
	MODULE VAR LPAREN inputs SEMICOLON outpus RPAREN LBRACE RBRACE {{
		modname = $2;
		inputs = $4;
		outputs = $6;
		}}*)
