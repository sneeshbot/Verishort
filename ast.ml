type op = Plus | Minus | Multiply | Modulus | Eq | Ne | Ge | Gt | Le | Lt | And | Or | Xor | Nand | Nor | Xnor | Lshift | Rshift | Not

type parameter = string * int * Lexing.position

type expr =
    DLiteral of int  * Lexing.position
  | BLiteral of string  * Lexing.position
  | Lvalue of lvalue  * Lexing.position
  | Binop of expr * op * expr * Lexing.position
  | Signext of int * expr * Lexing.position
  | Reduct of op * lvalue * Lexing.position
  | Unary of op * expr * Lexing.position
  | Concat of concat_item list * Lexing.position
	| Inst of string * binding_in list * binding_out list * Lexing.position
  | Reset of Lexing.position
  | Noexpr of Lexing.position
and concat_item = 
   | ConcatBLiteral of int * string
   | ConcatLvalue of int * lvalue
and lvalue = 
    Identifier of string
  | Subscript of string * expr
  | Range of string * expr * expr
and binding_in = lvalue * expr
and binding_out = lvalue * lvalue

type condition = Posedge | Negedge | Expression of expr

type statement = 
    Nop of Lexing.position
  | Expr of expr * Lexing.position
  | Block of statement list * Lexing.position
  | If of condition * statement * statement * Lexing.position
  | Case of lvalue * case_item list * Lexing.position
  | Return of expr * Lexing.position
  | For of expr * expr * expr * statement * Lexing.position
  | Assign of lvalue * expr * Lexing.position

and case_item = string * statement * Lexing.position

type decl_type = Wire | Reg

type declaration = {
	decltype : decl_type;
	declname : string;
	declwidth: int;
	init : expr;
	declpos : Lexing.position;
}

type id_with_width = string * int * Lexing.position


type mod_decl= {
	modname : string; (* Name of the module *)
	inputs : id_with_width list;
	outputs : id_with_width list;
	statements : statement list;
	parameters : parameter list;
	declarations: declaration list;
	returnwidth: int;
	modpos : Lexing.position;
}

type program = mod_decl list

exception Parse_Failure of string * Lexing.position
