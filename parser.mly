%{ open Ast %}

%token SEMICOLON LPAREN RPAREN LBRACE RBRACE COMMA COLON LBRACKET RBRACKET EOF
%token CASE CLOCK ELSE FOR IF INPUT MODULE NEGEDGE OUTPUT PARAMETER POSEDGE REG RESET RETURN WIRE
%token ASSIGN NOT OR XOR AND NOR XNOR NAND EQ NE GT GE LT LE LSHIFT RSHIFT PLUS MINUS MULTIPLY DIVIDE MODULUS SIGEXT
%token NOELSE
%token <string> ID
%token <int> LIT
%token <int> DECLIT
%token <string> XLIT
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR NOR
%left XOR XNOR
%left AND NAND
%left EQ NE
%left GT GE LT LE
%left LSHIFT RSHIFT
%left PLUS MINUS
%left MULTIPLY DIVIDE MODULUS 
%right SIGEXT NOT

%start program
%type <Ast.program> program 

%%

program:
			{[]}
	| program moddecl { $2 :: $1 } 		

moddecl:
	MODULE ID LPAREN INPUT formals_opt SEMICOLON OUTPUT formals_opt RPAREN LBRACE parameter_list decl_list stmt_list RBRACE {{
		modname = $2;
		inputs = $5;
		outputs = $8;
		statements = List.rev $13;
		parameters = $11;
		declarations = $12;
		}}
		
id_with_width: 
		ID LBRACKET DECLIT RBRACKET { { id = $1; width = $3;} }

id_with_width_opt:
		ID            { { id = $1; width = 1; } } 
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
	| decl_list decl { List.rev_append $2 $1 }

decl:
  WIRE wire_decl_with_opt_init_list SEMICOLON { $2 }
| REG reg_decl_with_opt_init_list SEMICOLON { $2 }

wire_decl_with_opt_init_list:
  wire_decl_with_opt_init { [$1] }
| wire_decl_with_opt_init_list COMMA wire_decl_with_opt_init { $3 :: $1 }

wire_decl_with_opt_init:
  ID { { decltype = Wire, declname = $1, decl_width = 1, init = NoExpr } }
| ID LBRACKET DECLIT RBRACKET { { decltype = Wire, declname = $1, decl_width = $3, init = NoExpr } }
| ID ASSIGN expr { { decltype = Wire, declname = $1, decl_width = 1, init = $3 } }
| ID LBRACKET DECLIT RBRACKET ASSIGN expr { { decltype = Wire, declname = $1, decl_width = $3, init = $6 } }

reg_decl_with_opt_init_list:
  reg_decl_with_opt_init { [$1] }
| reg_decl_with_opt_init_list COMMA reg_decl_with_opt_init { $3 :: $1 }

reg_decl_with_opt_init:
  ID { { decltype = Reg, declname = $1, decl_width = 1, init = NoExpr } }
| ID LBRACKET DECLIT RBRACKET { { decltype = Reg, declname = $1, decl_width = $3, init = NoExpr } }
| ID ASSIGN expr { { decltype = Reg, declname = $1, decl_width = 1, init = $3 } }
| ID LBRACKET DECLIT RBRACKET ASSIGN expr { { decltype = Reg, declname = $1, decl_width = $3, init = $6 } }

stmt_list:
		/* nothing */ { [] }
	| stmt_list stmt { $2 :: $1 }

stmt:
		expr SEMICOLON { }
	| RETURN expr SEMICOLON { }
	| LBRACE stmt_list RBRACE { }
	| IF LPAREN expr RPAREN stmt %prec NOELSE { }
	| IF LPAREN expr RPAREN stmt ELSE stmt { }
	| CASE LPAREN ID RPAREN LBRACE case_list RBRACE { }
	| FOR LPAREN expr_opt SEMICOLON expr_opt SEMICOLON expr_opt RPAREN stmt { }
	| SEMICOLON { } /* empty statements */

lvalue:
		ID { }
	| ID LBRACKET expr RBRACKET { }
	| ID LBRACKET expr COLON expr RBRACKET { }

expr:
		LIT { Literal($1) }
	| DECLIT { }
	| lvalue { }
	| lvalue ASSIGN expr { } 
	| expr PLUS expr { }
	| expr MINUS expr { }
	| expr MULTIPLY expr { }
	| expr DIVIDE expr { }
	| expr MODULUS expr { }
	| DECLIT SIGEXT expr { }
	| expr EQ expr { }
	| expr NE expr { }
	| expr GE expr { }
	| expr GT expr { }
	| expr LE expr { }
	| expr LT expr { }
	| expr AND expr { }
	| expr OR expr { }
	| expr XOR expr { }
	| expr NAND expr { }
	| expr NOR expr { }
	| expr XNOR expr { }
	| expr LSHIFT expr { }
	| expr RSHIFT expr { }
	| AND expr %prec NOT { } /* reductions */
	| OR expr %prec NOT { }
	| XOR expr %prec NOT { }
	| NAND expr %prec NOT { }
	| NOR expr %prec NOT { }
	| XNOR expr %prec NOT { }
	| RESET { }
	| LBRACE concat_list RBRACE { } /* Concatenation */
	| ID LPAREN binding_list SEMICOLON binding_list RPAREN { } /*Module instantiation */

expr_opt:
		/* nothing */ { }
	| expr { $1 }

concat_list:
		concat_item { }
	| concat_list COMMA concat_item { }

concat_item:
		LIT { }
	| lvalue { }
	| DECLIT LBRACE concat_item RBRACE { }
 
binding_list:
		/*Nothing */ { [] }
	| binding_list COMMA binding { $3 :: $1 }

binding:
		lvalue ASSIGN expr { }		
