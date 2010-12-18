open Int64

type im_op = ImPlus | ImMinus | ImMultiply | ImModulus | ImEq | ImNe | ImGe | ImLe | ImLt | ImAnd | ImOr | ImXor | ImNand | ImNor | ImXnor | ImLshift | ImRshift

type im_literal = int64 * int

type im_expr =
    ImLiteral of im_literal
  | ImLvalue of im_lvalue
  | ImBinop of im_expr * im_op * im_expr
  | ImReduct of im_op * im_lvalue
  | ImNot of im_expr
  | ImConcat of im_concat_item list
  | ImNoexpr
and im_concat_item = 
	ImConcatLit of int * im_literal
  | ImConcatLvalue of int * im_lvalue
and im_lvalue =
	ImIdentifier of string
  | ImSubscript of string * int
  | ImRange of string * int * int
  
type im_assignment = im_lvalue * im_expr

type im_always_condition = Always | Pos | Neg

type im_instantiation = string * im_assignment list * im_assignment list
type im_always = im_always_condition * im_always_stmt list
  
type im_always_stmt =
    ImAlwaysNop
  | ImAlwaysIf of im_expr * im_always_stmt list * im_always_stmt list
  | ImAlwaysCase of im_lvalue * im_case_item list
  | ImAlwaysRegAssign of im_lvalue * im_expr

and im_case_item = string * im_always_stmt list

type im_decl_type = ImWire | ImReg
type im_decl = im_decl_type * string * int;

type im_mod_decl = {
    im_modname : string;
    im_inputs : (string * int) list;
    im_outputs : (string * int) list;
    im_declarations : im_decl list;
    im_assignments : im_assignment list;
    im_instantiations : im_instantiations list;
    im_alwaysstmts : im_always list;
}

type im_program = im_mod_decl list
	
