open Ast
open Imst
open Parser
open Asttoimst
open Str

(*let rec invert_binary_actual n x = 
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
*)

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

let mod_id id = "_" ^ id

let op_to_string = function
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
	  ImConcatLit(replications,literal) -> raise (Failure("duh."))
	| ImConcatLvalue(replications, lv) -> string_of_int replications ^ "{" ^ (stringify_lvalue lv) ^ "}"

let stringify_concats lst = 
	let concat_string = List.map stringify_concat lst in

       "{" ^ (String.concat ", " concat_string) ^ "}"

(*let stringify_binary_literal (x,y) = (string_of_int y) ^ "'b" ^ decimal_to_binary_and_pad x y*)

let update_inst_count modname inst_map = 
	if StringMap.mem modname inst_map then StringMap.add modname ((StringMap.find modname inst_map) + 1) inst_map else StringMap.add modname 1 inst_map

let rec print_if_necessary str = function
   [] -> ["._" ^ str ^ "(_" ^ str ^ ")"]
   | (id, _) :: tl -> if id = str then [] else print_if_necessary str tl


let rec stringify_expression = function
	  ImLiteral(x,_) -> Int64.to_string x
	| ImLvalue(x) -> stringify_lvalue x
	| ImBinop(x, op, y) -> "(" ^ (stringify_expression x) ^ (op_to_string op) ^ (stringify_expression y ) ^ ")"
	| ImReduct(op, y) -> "(" ^ (op_to_string op) ^ (stringify_lvalue y) ^ ")"
	| ImUnary(op, expr) -> "(" ^ (op_to_string op) ^ (stringify_expression expr) ^ ")"
	| ImConcat(x) -> stringify_concats x
	| ImNoexpr -> ""

let print_inst inst_map (modname, bindlst1, bindlst2) = 
    let new_map = update_inst_count modname inst_map in
    let ind = StringMap.find modname new_map in
    print_string (modname ^ " ___" ^ modname ^ (string_of_int ind) ^ "(");
    print_string (String.concat ", " ((List.map (fun(id, exp) -> "._" ^ id ^ "(" ^ (stringify_expression exp) ^ ")") (bindlst1 @ bindlst2)) @ (
    print_if_necessary "clock" bindlst1) @ (print_if_necessary "reset" bindlst1)));
    print_endline ");";
    new_map
let rec print_case (b, stmt) = print_endline ((string_of_int (String.length b)) ^ "'b" ^ b ^ ": "); print_endline "begin"; List.iter print_statement stmt; print_endline "end"
and print_case_list lst = List.iter print_case lst
and print_statement = function
	  ImNop -> ()
	| ImIf(pred,tru,fal) -> print_endline ("if (" ^ (stringify_expression pred) ^ ")"); print_endline "begin"; List.iter print_statement tru; print_endline "end\nelse\nbegin"; List.iter print_statement fal; print_endline "end"
	| ImCase(lv,csl) -> print_endline ("casex(" ^ (stringify_lvalue lv) ^ ")"); print_case_list csl; print_endline "endcase"
	| ImRegAssign(lv, expr) -> print_string (stringify_lvalue lv); print_string "="; print_string (stringify_expression expr); print_endline ";"

let print_module_sig m = 
  (* print module sig *)
  let mod_args = (m.im_inputs, m.im_outputs) in
  let mod_arg_list (inputs, outputs) = 
  String.concat ", " (List.map (fun (name, _) -> "_" ^ name) (inputs @ outputs)) in
  (print_string ("module _" ^ m.im_modname ^ "(" ^ (mod_arg_list mod_args) ^ ");\n");
  List.iter (fun (id, width ) -> print_string ("input " ^ (if width == 1 then "" else ("[" ^ string_of_int (width - 1) ^ ":0] ")) ^ "_" ^ id ^ ";\n")) (fst mod_args);
  List.iter (fun (id, width ) -> print_string ("output " ^ (if width == 1 then "" else ("[" ^ string_of_int (width - 1) ^ ":0] ")) ^ "_" ^ id ^ ";\n")) (snd mod_args))
  
  (* print decls *)
let print_decl (typ, str, width) = print_endline ((if typ = ImWire then "wire" else "reg") ^ " " ^ (if width == 1 then "" else ("[" ^ (string_of_int (width - 1)) ^ ":0] ")) ^ "_" ^ str ^ ";\n")  

let print_assignment (lv, expr) = print_string ("assign " ^ (stringify_lvalue lv)); print_string (" = " ^ (stringify_expression expr)); print_endline ";"

(*let rec replace_char str char_orig char_new pos = 
	try while true; do 
		replace_char (str.(String.index_from str pos char_orig) <- char_new) char_orig char_new (pos+1)
	done
	with Not_found -> str

let print_lib libname width libmap = 
	if StringMap.mem libname libmap then StringMap.add libname ((StringMap.find libname libmap) + 1) libmap else StringMap.add libname 1 libmap;
	
	let libnum = StringMap.find libname libmap in
	
	let code = "" in
	let chan = open_in (libname ^ ".v") in
	try
		while true; do
			code = (code ^ (input_line chan) ^ "\n")
		done;
	with End_of_file -> close_in chan
		
	replace_char code '#' ((string_of_int libnum).(0)) 0;
	
	print_endline code;
*)

let print_libmod libname libwidth actualname = 			
	let filename = (Filename.current_dir_name ^ "/stdlib/" ^ libname ^ ".v") in
	let chan = open_in filename in
	try
		while true; do
			let rawline = input_line chan in
			let replname = Str.global_replace (Str.regexp_string libname) actualname rawline in
			let replwidth = Str.global_replace (Str.regexp_string "WIDTHMINUSONE") (string_of_int (libwidth - 1)) replname in
			let repllogwidth = Str.global_replace (Str.regexp_string "LOGWIDTH") (string_of_int (get_min_bit_width (Int64.of_int libwidth))) replwidth in
			print_endline repllogwidth 
		done;
	with End_of_file -> close_in chan
		
	
	
	(*ignore(StringSet.add libname libset)*)
	
	(*else ()*)
	
	
let print_module m =
	let libset = StringSet.empty in  	
	if m.im_libmod then print_libmod m.im_libmod_name m.im_libmod_width m.im_modname else (
   print_module_sig m;
   List.iter print_decl m.im_declarations;
   List.iter print_assignment m.im_assignments;
   ignore (List.fold_left print_inst StringMap.empty m.im_instantiations);
   print_endline "always @ (*) begin";
   List.iter print_statement m.im_alwaysall;
   print_endline "if (_reset) begin";
   List.iter (fun (typ, name, _) -> if typ = ImReg then print_endline ("_" ^ name ^ "= 0;") else ()) m.im_declarations;
   print_endline "end\nend";
   print_endline "always @ (posedge _clock) begin";
   List.iter print_statement m.im_alwaysposedge;
   print_endline "end";
   print_endline "always @ (negedge _clock) begin";
   List.iter print_statement m.im_alwaysnegedge;
   print_endline "end";
   print_endline "endmodule")
   
   
   
let _ =
  let inname = if Array.length Sys.argv > 1 then Sys.argv.(1) else "stdin" in
  let inchannel = if Array.length Sys.argv > 1 then Pervasives.open_in Sys.argv.(1) else stdin in
  let lexbuf = Lexing.from_channel inchannel in
  try 
	  let sourcecode = List.rev (Parser.program Scanner.token lexbuf) in
	    List.iter  print_module (translate sourcecode)
  with Parse_Failure(msg, pos) -> print_endline (inname ^ ":" ^ (string_of_int pos.Lexing.pos_lnum) ^":" ^ (string_of_int (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)) ^": " ^ msg )
