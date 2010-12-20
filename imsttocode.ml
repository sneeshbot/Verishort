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
	  ImIdentifier(id) ->  (mod_id id)
	| ImSubscript(id, ind) -> ((mod_id id) ^ "[" ^ string_of_int ind ^ "]")

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

let print_assignment (lv, expr) = print_string "assign"; print_lvalue lv; print_string "="; print_expression expr;

let print_assignments asses = List.iter print_assignment asses

