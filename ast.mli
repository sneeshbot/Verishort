type op = PLUS | MINUS | MULTIPLY | DIVIDE | MODULUS | EQ | NE | GE | GT | LE | LT | AND | OR | XOR | NAND | NOR | XNOR | LSHIFT | RSHIFT


type moddecl= {
	modname : string; (* Name of the module *)
	inputs : string list;
	outputs : string list;
	statements : statement list;
	parameters : parameter list;
	declarations: declaration list;
	}

type program = mod_decl list

type parameter = 
	

type statement

type decl_type = Wire | Reg

type declaration = {
	decltype : decl_type;
	declname : string;
	declwidth: int;
	init : expr;
}

type expr =
    Literal of int
  | Id of string
  | Binop of expr * op * expr
  | Assign of string * expr
  | Signext of int * expr
  | Reduct of op * expr
  | Noexpr

type lvalue

type id_with_width = {
	id : string;
	width : int;
}


