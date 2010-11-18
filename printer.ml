open Ast

let op_to_string = function
	  Plus   -> "+" 
	| Minus  -> "-"
	| Multiply  -> "*"
	| Divide -> "/"
	| Modulus-> "%"
	| Eq     -> "=="
	| Ne     -> "!="
	| Ge     -> ">="
	| Gt     -> ">"
	| Le     -> "<="
	| Lt     -> "<"
	| And    -> "&"
	| Or     -> "|"
	| Xor    -> "^"
	| Nand   -> "!&"
	| Nor    -> "!|"
	| Xnor   -> "!^"
	| Lshift -> "<<"
	| Rshift -> ">>"

let rec print_expression = function
	  Noexpr -> print_endline "Expression: No expression"
	| Reset -> print_string "Reset"
	| DLiteral(x)  ->  print_string ((string_of_int x) ^ "d") 
	| BLiteral(x)  -> print_string (x ^ "b")
	| Lvalue(x) -> print_lvalue(x)
	| Binop(x, op, y) -> print_string "("; print_expression x; print_string (op_to_string op); print_expression y; print_string ")"
	| Signext(x, y) -> print_int x; print_string "'"; print_expression y
	| Assign(x, y) -> print_lvalue x; print_string ":="; print_expression y
	| Reduct(op, y) -> print_string (op_to_string op); print_expression y 
	| Concat(x) -> print_string "concat("; print_concats x; print_string ")"
	| Inst(x, input, output) -> print_string (x^"("); print_bindings input; print_string "; "; print_bindings output; print_string ")"
and print_lvalue = function
		Identifier(id) -> print_string id
	| Subscript(id, ind) -> print_string (id ^ "["); print_expression ind; print_string "]"
	| Range(id, ind1, ind2) -> print_string (id ^ "["); print_expression ind1; print_string ":"; print_expression ind2; print_string "]"
and print_binding (a, b) = print_lvalue a; print_string "="; print_expression b
and print_bindings = function
	  [] -> ()
	| [a] -> print_binding a
	| hd::tl -> print_binding hd; List.iter (fun a -> print_string ","; print_binding a) tl
and print_concats = function
	  [] -> ()
	| [a] -> print_concat a
	| hd::tl -> print_concat hd; List.iter (fun a -> print_string ","; print_concat a) tl
and print_concat = function
	  ConcatBLiteral(x) -> print_string (x ^ "b")
	| ConcatLvalue(x) -> print_lvalue x
	| ConcatDuplBLiteral(n, x) -> print_int n; print_string ("{" ^ x ^ "b}")
	| ConcatDuplLvalue(n, x) -> print_int n; print_string "{"; print_lvalue x; print_string "}"

let rec print_statement = function
		Nop -> print_endline "Statement: Nop"
	| Expr(exp) -> print_endline "Statement: Expr: "; print_expression exp; print_newline (); print_endline "End expression statement"
	| Block(ls) -> print_endline "Statement: Block:"; List.iter print_statement ls; print_endline "End block statement"
	| Return(exp) -> print_endline "Statement: Return: "; print_expression exp; print_newline (); print_endline "End return statement"
	| If(pred, tru, fal) -> print_endline "Statement: If: "; print_expression pred;
			 print_newline (); print_endline "Then: "; print_statement tru; print_endline "Else:"; print_statement fal; print_newline (); print_endline "End if statement"
	| Case(var, lst) -> print_string "Statement: Case: "; print_lvalue var; print_newline (); print_case_list lst; print_endline "End case statement"
	| For(exp1, exp2, exp3, stmt) -> print_endline "Statement: For: "; print_expression exp1; print_string "; "; 
				print_expression exp2; print_string "; "; print_expression exp3; print_newline ();
				print_statement stmt; print_endline "End for statement" 
and print_case (b, stmt) = print_string (b^"b"); print_statement stmt
and print_case_list list = List.iter print_case list

let print_decltype = function
	  Wire -> print_string "wire"
	| Reg -> print_string "register"

let print_decl x =
	print_decltype x.decltype; print_string (" " ^ x.declname ^ "[");
	print_int x.declwidth; print_string "]"; (if x.init != Noexpr then print_string " = "; print_expression x.init);
	print_endline ";" 
let print_module l = 
	print_endline ("module " ^ l.modname ^ "[" ^ (string_of_int l.returnwidth) ^ "]");
	print_endline "Inputs: "; List.iter (fun a -> (print_string (a.id ^ "["); print_int a.width; print_endline "]")) l.inputs;
	print_endline "Outputs: "; List.iter (fun a -> (print_string (a.id ^ "["); print_int a.width; print_endline "]")) l.outputs;
	print_endline "Parameters: "; List.iter (fun (a, b)  -> print_string (a ^ "="); print_endline (string_of_int b)) l.parameters;
	print_endline "Declarations:"; List.iter print_decl l.declarations;
	print_endline "Statements:"; List.iter print_statement l.statements;
	print_endline "end module"

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let sourcecode = Parser.program Scanner.token lexbuf in
  List.iter print_module sourcecode
		