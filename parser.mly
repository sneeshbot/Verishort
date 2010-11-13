%{ open Ast %}

%token COMMENT SEMICOLON LPAREN RPAREN LBRACE RBRACE COMMA MODULE INPUT OUTPUT LBRACKET RBRACKET WIRE REG PARAMETER RETURN
%token <string> ID
%token <int> LIT
%token <int> DECLIT
%token EOF

%start sourcecode
%type <Ast.sourcecode> sourcecode 

%%

sourcecode:
			{[]}
	| sourcecode moddecl { $2 :: $1 } 		

moddecl:
	MODULE ID LPAREN INPUT formals_opt SEMICOLON OUTPUT formals_opt RPAREN LBRACE parameter_list decl_list RBRACE {{
		modname = $2;
		inputs = $5;
		outputs = $8;
		}}
		
id_with_width: 
		ID LBRACKET DECLIT RBRACKET { /*Whatever AST we decide upon*/ }

id_with_width_opt:
		ID            { /*Whatever AST type we decide upon*/ } 
	| id_with_width { $1 }

id_with_width_opt_list:
		id_with_width_opt { [$1] }
	| id_with_width_opt_list COMMA id_with_width_opt { $3 :: $1 }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    id_with_width_opt_list { $1 }

parameter_list:
		/* nothing */ { [] }
	| parameter_list parameter_decl { List.rev_append $2 $1 }

parameter_decl:
		PARAMETER parameter_initialization_list SEMICOLON { $2 }

parameter_initialization_list:
		parameter_initialization { [$1] }
	| parameter_initialization_list COMMA parameter_initialization { $3 :: $1 }

parameter_initialization:
		ID ASSIGN DECLIT { /* whatever ast ... */ }

decl_list:
		/* nothing */ { [] }
	| decl_list decl { $2 :: $1 }

decl:
    WIRE id_with_width_opt_list SEMICOLON { /* ... */ }
	| REG id_with_width_opt_list SEMICOLON { /* ... */ }
	
