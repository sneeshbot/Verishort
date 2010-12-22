open Imst
open Compile

let rec invert_binary_actual n x = 
  if n < 0 then x else
    (x.[n] <- (if x.[n] = '1' then '0' else '1'); invert_binary_actual (n-1) x)
	and invert_binary x = invert_binary_actual (String.length x - 1) (String.copy x)

	let subtract_one bin bits= 
		decimal_to_binary (Int64.pred (Int64.from_string ("0b" ^ bin))) bits

let rec dec_conv d [] = match d with
	  0 -> lst
	| _ -> dec_conv (d/2)(d mod 2)::lst

let rec pad_binary b bits = 
	if List.length b = bits then b
	else pad_binary 0::b

let decimal_to_binary_and_pad d bits =
	let is_negative = (Int64.compare d 0 < 0) in

	let bin = dec_conv (if is_negative then Int64.neg d else d) in

	let bin_string = List.fold_left string_of_int "" (pad_binary bin bits) in

	if is_negative then subtract_one(invert_binary bin_string) bits else bin_string

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
	| ImNand -> "!&"
	| ImNor -> "!|"
	| ImXnor -> "!^"
	| ImLshift -> "<<"
	| ImRshift -> ">>"
	| ImNot -> "" (* TODO *)

let stringify_lvalue = function
	ImSubscript(id, ind) -> ((mod_id id) ^ "[" ^ string_of_int ind ^ "]")

let stringify_concat = function
	  ImConcatLit(replications,literal) -> replications ^ "{" ^ (stringify_literal literal)  ^ "}"
	| ImConcatLvalue(replications, lv) -> replications ^ "{" ^ (stringify_lvalue lv) ^ "}"

let print_concats lst = 
	let concated = List.map stringify_concat lst in
	let commaed = List.map (fun s -> s ^ ",") lst in 
	print_string ("{" ^ (String.sub commaed 0 ((String.length commaed) - 1)) ^ "}")

let stringify_literal (x,y) = decimal_to_binary_and_pad x y

let rec print_expression = function
	  ImLiteral(x,y) -> print_string(stringify_literal (x,y))
	| ImLValue(x,_) -> print_lvalue(stringify_lvalue x)
	| ImBinop(x, op, y,_) -> print_string "("; print_expression x; print_string(op_to_string op); print_expression y; print_string ")";
	| ImReduct(op, y,_) -> print_string (op_to_string op); print_lvalue y
	| ImUnary() (* TODO *)
	| ImConcat(x,_) -> print_string "concat("; print_concats x; print_string ")"
	| ImNoexpr(_) -> print_endline ""

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