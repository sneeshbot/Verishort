open Imst
open Compile

let rec invert_binary_actual n x = 
  if n < 0 then x else
    (x.[n] <- (if x.[n] = '1' then '0' else '1'); invert_binary_actual (n-1) x)
	and invert_binary x = invert_binary_actual (String.length x - 1) (String.copy x)

let rec add_one_actual n x =
  if n < 0 then x (*discard overflow bit*)
  else
  if x.[n] = '1' then (x.[n] <- '0'; add_one_actual (n-1) x) else (x.[n] <- '1'; x)
and add_one x = add_one_actual (String.length x -1) (String.copy x)



let rec dec_conv d str = 
	if (Int64.compare d Int64.zero) = 0 then str else dec_conv (Int64.shift_right d 1) (Int64.to_string (Int64.rem d (Int64.of_int 2)) ^ str)

let rec pad_binary b bits = 
	if List.length b = bits then b
	else pad_binary 0::b

let decimal_to_binary_and_pad d bits =
	let is_negative = (Int64.compare d Int64.zero < 0) in

	let bin_string = dec_conv (if is_negative then Int64.neg d else d) in

	if is_negative then add_one(invert_binary bin_string) bits else bin_string

let mod_id id = "_" ^ id

let im_op_to_string = function
	  ImPlus -> "+"
	| ImMinus -> "-"
	| ImMultiply -> "*"
	| ImModulus -> "%"
	| ImEq -> "=="
	| ImNe -> "!="
	| ImGe -> ">="
	| ImGt -> ">"
	| ImLe -> "<="
	| ImLt -> "<"
	| ImAnd -> "&"
	| ImOr -> "|"
	| ImXor -> "^"
	| ImNand -> "~&"
	| ImNor -> "~|"
	| ImXnor -> "~^"
	| ImLshift -> "<<"
	| ImRshift -> ">>"
	| ImNot -> "~"

let stringify_lvalue = function
	  ImSubscript(id, ind) -> ((mod_id id) ^ "[" ^ (string_of_int ind) ^ "]")
	| ImRange(id, ind1, ind2) -> ((mod_id id) ^ "[" ^ (string_of_int ind1) ^ ":" ^ (string_of_int ind2) ^ "]")

let stringify_concat = function
	  ImConcatLit(replications,literal) -> string_of_int replications ^ "{" ^ (stringify_binary_literal literal)  ^ "}"
	| ImConcatLvalue(replications, lv) -> string_of_int replications ^ "{" ^ (stringify_lvalue lv) ^ "}"

let print_concats lst = 
	let concat_string = List.map stringify_concat lst in

	print_string ("{" ^ (String.concat ", " concat_string) ^ "}")

let stringify_binary_literal (x,y) = (string_of_int y) ^ "'b" ^ decimal_to_binary_and_pad x y

let rec print_expression = function
	  ImLiteral(x,y) -> print_string(stringify_binary_literal (x,y))
	| ImLValue(x) -> print_lvalue(stringify_lvalue x)
	| ImBinop(x, op, y) -> print_string "("; print_expression x; print_string(op_to_string op); print_expression y; print_string ")";
	| ImReduct(op, y) -> print_string ("(" ^ (op_to_string op)); print_lvalue y; print_string ")"
	| ImUnary(op, expr) -> print_string ("(" ^ (op_to_string op)); print_expression expr; print_string ")" 
	| ImConcat(x) -> print_string "concat("; print_concats x; print_string ")"
	| ImNoexpr -> ()

let print_assignments asses = List.iter print_assignment asses

let print_case (b, stmt, _) = print_string(b ^ "b: "); print_statement stmt
let print_case_list list = List.iter print_case list

let print_statement = function
	  ImNop -> ()
	| ImIf(pred,tru,fal) -> print_endline "begin"; print_string "if"; print_string("(" ^ print_expression im_expr ^ ")"); print_newline (); List.iter print_statement tru; print_endline "else"; List.iter print_statement fal; print_endline "end"
	| ImCase(lv,csl) -> print_endline "case"; print_string("(" ^ print_expression lv ^ ")"); print_newline (); print_case list csl; print_endline "endcase"
	| ImRegAssign(lv, expr) -> print_string lv; print_string "="; print_expression expr

let print_module_sig m = 
  (* print module sig *)
  let mod_args = (m.im_inputs, m.im_outputs) in
  let mod_arg_list (inputs, outputs) = 
  String.concat ", " (List.map (fun (name, _) -> name) (inputs @ outputs)) in
  (output_string out_file ("module " ^ m.im_modname ^ "(" ^ (mod_arg_list mod_args) ^ ");\n");
  List.iter (fun (id, width ) -> output_string out_file ("input " ^ (if width == 1 then "" else ("[" ^ string_of_int (width - 1) ^ ":0] ")) ^ id ^ ";\n")) (fst mod_args);
  List.iter (fun (id, width ) -> output_string out_file ("output " ^ (if width == 1 then "" else ("[" ^ string_of_int (width - 1) ^ ":0] ")) ^ id ^ ";\n")) (snd mod_args))
  
  (* print decls *)
let print_decl (typ, str, width) = print_endline ((if typ = ImWire then "wire" else "reg") ^ " " ^ (if width == 1 then "" else ("[" ^ (string_of_int (width - 1)) ^ ":0] ")) ^ str ^ ";\n")  

let print_assignment (lv, expr) = print_string "assign "; print_lvalue lv; print_string " = "; print_expression expr; print_endline ";"
