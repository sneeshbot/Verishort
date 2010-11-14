type op = Plus | Minus | Multiply | Divide | Modulus | Eq | Ne | Ge | Gt | Le | Lt | And | Or | Xor | Nand | Nor | Xnor | Lshift | Rshift

type parameter = string * int

type expr =
    DLiteral of int
  | BLiteral of string
  | Lvalue of lvalue
  | Binop of expr * op * expr
  | Assign of lvalue * expr
  | Signext of int * expr
  | Reduct of op * expr
  | Concat of concat_item list
  | Inst of string * binding list * binding list
  | Reset
  | Noexpr
and concat_item = 
     ConcatBLiteral of string
   | ConcatLvalue of lvalue
   | ConcatDuplBLiteral of int * string
   | ConcatDuplLvalue of int * lvalue
and lvalue = 
    Identifier of string
  | Subscript of string * expr
  | Range of string * expr * expr
and binding = lvalue * expr


type statement = 
    Nop
  | Expr of expr
  | Block of statement list
  | If of expr * statement * statement
  | Case of lvalue * case_item list
  | Return of expr
  | For of expr * expr * expr * statement

and case_item = string * statement

type decl_type = Wire | Reg

type declaration = {
	decltype : decl_type;
	declname : string;
	declwidth: int;
	init : expr;
}

type id_with_width = {
	id : string;
	width : int;
}

type moddecl= {
	modname : string; (* Name of the module *)
	inputs : string list;
	outputs : string list;
	statements : statement list;
	parameters : parameter list;
	declarations: declaration list;
	returnwidth: int;
}

type program = mod_decl list
