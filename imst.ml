open Int64

type im_op = ImPlus | ImMinus | ImMultiply | ImModulus | ImEq | ImNe | ImGe | ImLe | ImLt | ImGt | ImAnd | ImOr | ImXor | ImNand | ImNor | ImXnor | ImLshift | ImRshift | ImNot

type im_literal = int64 * int

type im_expr =
    ImLiteral of im_literal
  | ImLvalue of im_lvalue
  | ImBinop of im_expr * im_op * im_expr
  | ImReduct of im_op * im_lvalue
  | ImUnary of im_op * im_expr
  | ImConcat of im_concat_item list
  | ImNoexpr
and im_concat_item = 
	  ImConcatLit of int * im_literal
  | ImConcatLvalue of int * im_lvalue
and im_lvalue =
    ImSubscript of string * int
  | ImRange of string * int * int
  
type im_assignment = im_lvalue * im_expr
type im_binding = string * im_expr
type im_instantiation = string * im_binding list * im_binding list
  
type im_always_stmt =
    ImNop
  | ImIf of im_expr * im_always_stmt list * im_always_stmt list
  | ImCase of im_lvalue * im_case_item list
  | ImRegAssign of im_lvalue * im_expr

and im_case_item = string * im_always_stmt list

type im_decl_type = ImWire | ImReg

type im_decl = im_decl_type * string * int

type im_mod_decl = {
    im_modname : string;
		im_libmod : bool;
		im_libmod_name: string;
		im_libmod_width: int;
    im_inputs : (string * int) list;
    im_outputs : (string * int) list;
    im_declarations : im_decl list;
    im_assignments : im_assignment list;
    im_instantiations : im_instantiation list;
    im_alwaysall : im_always_stmt list;
		im_alwaysposedge : im_always_stmt list;
		im_alwaysnegedge : im_always_stmt list;
}

type im_program = im_mod_decl list
