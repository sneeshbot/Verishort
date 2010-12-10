%{ open Ast
%}

%token SEMICOLON LPAREN RPAREN LBRACE RBRACE COMMA COLON LBRACKET RBRACKET EOF
%token CASE CLOCK CONCAT ELSE FOR IF INPUT MODULE NEGEDGE OUTPUT PARAMETER POSEDGE REG RESET RETURN WIRE
%token ASSIGN NOT OR XOR AND NOR XNOR NAND EQ NE GT GE LT LE LSHIFT RSHIFT PLUS MINUS MULTIPLY DIVIDE MODULUS SIGEXT
%token NOELSE
%token <string> ID
%token <int> DLIT
%token <int> DECLIT
%token <string> BLIT
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
	  /* nothing */	  {[]}
	| program moddecl { $2 :: $1 } 		

moddecl:
	MODULE ID LPAREN input_output RPAREN LBRACE parameter_list decl_list stmt_list RBRACE {{
		modname = $2;
		inputs = fst $4;
		outputs = snd $4;
		statements = List.rev $9;
		parameters = $7;
		declarations = $8;
		returnwidth = 0;
		}}
 | MODULE ID LBRACKET DECLIT RBRACKET LPAREN input_output RPAREN LBRACE parameter_list decl_list stmt_list RBRACE {{
		modname = $2;
		inputs = fst $7;
		outputs = snd $7;
		statements = List.rev $12;
		parameters = $10;
		declarations = $11;
		returnwidth = $4;
		}}

input_output:
			INPUT formals_opt  { $2, [] }
		| OUTPUT formals_opt { [], $2 }
		| INPUT formals_opt SEMICOLON OUTPUT formals_opt { $2, $5 }
		| error { raise (Parse_Failure("Module arguments parsing error." , Parsing.symbol_start_pos () )) }
		
id_with_width: 
		ID LBRACKET DECLIT RBRACKET { $1, $3 }

id_with_width_opt:
		ID            { $1, 1 } 
	| id_with_width { $1 }

id_with_width_opt_list:
		id_with_width_opt { [$1] }
	| id_with_width_opt_list COMMA id_with_width_opt { $3 :: $1 }

formals_opt:
    /* nothing */ { [] }
  | id_with_width_opt_list   { List.rev $1 }

parameter_list:
		/* nothing */ { [] }
	| parameter_list parameter_decl { List.rev_append $2 $1 }

parameter_decl:
		PARAMETER parameter_initialization_list SEMICOLON { $2 }

parameter_initialization_list:
		parameter_initialization { [$1] }
	| parameter_initialization_list COMMA parameter_initialization { $3 :: $1 }

parameter_initialization:
		ID ASSIGN DECLIT { $1, $3 }

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
  	ID { { decltype = Wire; declname = $1; declwidth = 1; init = Noexpr } }
	| ID LBRACKET DECLIT RBRACKET { { decltype = Wire; declname = $1; declwidth = $3; init = Noexpr } }
	| ID ASSIGN expr { { decltype = Wire; declname = $1; declwidth = 1; init = $3 } }
	| ID LBRACKET DECLIT RBRACKET ASSIGN expr { { decltype = Wire; declname = $1; declwidth = $3; init = $6 } }

reg_decl_with_opt_init_list:
  	reg_decl_with_opt_init { [$1] }
	| reg_decl_with_opt_init_list COMMA reg_decl_with_opt_init { $3 :: $1 }

reg_decl_with_opt_init:
  	ID { { decltype = Reg; declname = $1; declwidth = 1; init = Noexpr } }
	| ID LBRACKET DECLIT RBRACKET { { decltype = Reg; declname = $1; declwidth = $3; init = Noexpr } }
	| ID ASSIGN expr { { decltype = Reg; declname = $1; declwidth = 1; init = $3 } }
	| ID LBRACKET DECLIT RBRACKET ASSIGN expr { { decltype = Reg; declname = $1; declwidth = $3; init = $6 } }

stmt_list:
		/* nothing */ { [] }
	| stmt_list stmt { $2 :: $1 }

stmt:
		expr SEMICOLON { Expr($1) }
	| RETURN expr SEMICOLON { Return($2) }
	| LBRACE stmt_list RBRACE { Block(List.rev $2) }
	| IF LPAREN condition RPAREN stmt %prec NOELSE { If($3, $5, Nop) }
	| IF LPAREN condition RPAREN stmt ELSE stmt { If($3, $5, $7) }
	| CASE LPAREN lvalue RPAREN LBRACE case_list RBRACE { Case($3, List.rev $6) }
	| FOR LPAREN expr_opt SEMICOLON expr_opt SEMICOLON expr_opt RPAREN stmt { For($3, $5, $7, $9) }
	| SEMICOLON { Nop } /* empty statements */

condition:
		POSEDGE { Posedge }
	| NEGEDGE { Negedge }
	| expr { Expression($1) }

case_list:
		case_item { [$1] }
	| case_list case_item { $2 :: $1 }

case_item: 
		BLIT COLON stmt { $1, $3 }
	| XLIT COLON stmt { $1, $3 }

lvalue:
		ID { Identifier($1) }
	| ID LBRACKET expr RBRACKET { Subscript($1, $3) }
	| ID LBRACKET expr COLON expr RBRACKET { Range($1, $3, $5) }

expr:
		DLIT { DLiteral($1) }
	| BLIT { BLiteral($1) }
	| DECLIT { DLiteral($1) }
	| lvalue { Lvalue($1) }
	| lvalue ASSIGN expr { Assign($1, $3) } 
	| expr PLUS expr { Binop($1, Plus, $3) }
	| expr MINUS expr { Binop($1, Minus, $3) }
	| expr MULTIPLY expr { Binop($1, Multiply, $3) }
	| expr DIVIDE expr {Binop($1, Divide, $3) }
	| expr MODULUS expr {Binop($1, Modulus, $3)}
	| DECLIT SIGEXT expr {  Signext($1, $3) }
	| expr EQ expr { Binop($1, Eq, $3)}
	| expr NE expr {Binop($1, Ne, $3) }
	| expr GE expr {Binop($1, Ge, $3) }
	| expr GT expr {Binop($1, Gt, $3)}
	| expr LE expr {Binop($1, Le, $3) }
	| expr LT expr {Binop($1, Lt, $3) }
	| expr AND expr { Binop($1, And, $3)}
	| expr OR expr {Binop($1, Or, $3) }
	| expr XOR expr {Binop($1, Xor, $3) }
	| expr NAND expr {Binop($1, Nand, $3) }
	| expr NOR expr {Binop($1, Nor, $3) }
	| expr XNOR expr { Binop($1, Xnor, $3)}
	| expr LSHIFT expr {Binop($1, Lshift, $3) }
	| expr RSHIFT expr { Binop($1, Rshift, $3)}
	| AND expr %prec NOT {Reduct(And, $2) } /* reductions */
	| OR expr %prec NOT {Reduct(Or, $2) }
	| XOR expr %prec NOT {Reduct(Xor, $2) }
	| NAND expr %prec NOT {Reduct(Nand, $2) }
	| NOR expr %prec NOT {Reduct(Nor, $2) }
	| XNOR expr %prec NOT {Reduct(Xnor, $2) }
	| RESET { Reset }
	| CONCAT LPAREN concat_list RPAREN { Concat(List.rev $3) } /* Concatenation */
	| ID LPAREN binding_list_opt SEMICOLON binding_list_opt RPAREN { Inst($1, List.rev $3, List.rev $5) } /*Module instantiation */

expr_opt:
		/* nothing */ { Noexpr }
	| expr { $1 }

concat_list:
		concat_item { [$1] }
	| concat_list COMMA concat_item { $3 :: $1 }

concat_item:
    BLIT { ConcatBLiteral($1) }
	| lvalue { ConcatLvalue($1) }
	| DECLIT LBRACE BLIT RBRACE { ConcatDuplBLiteral($1, $3) } /* duplicated blit */
	| DECLIT LBRACE lvalue RBRACE { ConcatDuplLvalue($1, $3) } /* duplicated lvalue */
 
binding_list:
	binding { [$1] }
	| binding_list COMMA binding { $3 :: $1 }

binding_list_opt:
	/*nothing*/ { [] }
	| binding_list { $1 }

binding:
		lvalue ASSIGN expr { $1, $3 }		
