open Ast
open Compile
(*exception Parse_Failure of string * Lexing.position*)

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
	  Noexpr(_) -> print_endline "Expression: No expression"
	| Reset(_) -> print_string "Reset"
	| DLiteral(x,_)  ->  print_string ((string_of_int x) ^ "d") 
	| BLiteral(x,_)  -> print_string (x ^ "b")
	| Lvalue(x,_) -> print_lvalue(x)
	| Binop(x, op, y,_) -> print_string "("; print_expression x; print_string (op_to_string op); print_expression y; print_string ")"
	| Signext(x, y,_) -> print_int x; print_string "'"; print_expression y
	| Assign(x, y,_) -> print_lvalue x; print_string ":="; print_expression y
	| Not(x,_) -> print_string "!"; print_expression x
	| Reduct(op, y,_) -> print_string (op_to_string op); print_lvalue y 
	| Concat(x,_) -> print_string "concat("; print_concats x; print_string ")"
	| Inst(x, input, output,_) -> print_string (x^"("); print_bindings input; print_string "; "; print_bindings output; print_string ")"
	
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

let print_condition = function
	  Posedge -> print_string "posedge"
	| Negedge -> print_string "negedge"
	| Expression(exp) -> print_expression exp

let rec print_statement = function
		Nop(_) -> print_endline "Statement: Nop"
	| Expr(exp,_) -> print_endline "Statement: Expr: "; print_expression exp; print_newline (); print_endline "End expression statement"
	| Block(ls,_) -> print_endline "Statement: Block:"; List.iter print_statement ls; print_endline "End block statement"
	| Return(exp,_) -> print_endline "Statement: Return: "; print_expression exp; print_newline (); print_endline "End return statement"
	| If(pred, tru, fal,_) -> print_endline "Statement: If: "; print_condition pred;
			 print_newline (); print_endline "Then: "; print_statement tru; print_endline "Else:"; print_statement fal; print_endline "End if statement"
	| Case(var, lst,_) -> print_string "Statement: Case: "; print_lvalue var; print_newline (); print_case_list lst; print_endline "End case statement"
	| For(exp1, exp2, exp3, stmt,_) -> print_endline "Statement: For: "; print_expression exp1; print_string "; "; 
				print_expression exp2; print_string "; "; print_expression exp3; print_newline ();
				print_statement stmt; print_endline "End for statement" 
and print_case (b, stmt,_) = print_string (b^"b: "); print_statement stmt
and print_case_list list = List.iter print_case list

let print_decltype = function
	  Wire -> print_string "wire"
	| Reg -> print_string "register"

let print_decl x =
	print_decltype x.decltype; print_string (" " ^ x.declname ^ "[");
	print_int x.declwidth; print_string "]"; print_string " = "; print_expression x.init;
	print_endline ";" 
	
let print_module l = 
	print_endline ("module " ^ l.modname ^ "[" ^ (string_of_int l.returnwidth) ^ "]");
	print_endline "Inputs: "; List.iter (fun (id, width,_) -> (print_string (id^ "["); print_int width; print_endline "]")) l.inputs;
	print_endline "Outputs: "; List.iter (fun (id,width,_) -> (print_string (id ^ "["); print_int width; print_endline "]")) l.outputs;
	print_endline "Parameters: "; List.iter (fun (a, b,_)  -> print_string (a ^ "="); print_endline (string_of_int b)) l.parameters;
	print_endline "Declarations:"; List.iter print_decl l.declarations;
	print_endline "Statements:"; List.iter print_statement l.statements;
	print_endline "end module"

let _ =
  let inname = if Array.length Sys.argv > 1 then Sys.argv.(1) else "stdin" in
  let inchannel = if Array.length Sys.argv > 1 then Pervasives.open_in Sys.argv.(1) else stdin in
  let lexbuf = Lexing.from_channel inchannel in
  try 
	  let sourcecode = List.rev (Parser.program Scanner.token lexbuf) in
  		List.iter print_module (sourcecode);
  		print_endline "********";
  		translate sourcecode (*print_decl*)
  with Parse_Failure(msg, pos) -> print_endline (inname ^ ":" ^ (string_of_int pos.Lexing.pos_lnum) ^":" ^ (string_of_int (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)) ^": " ^ msg )
