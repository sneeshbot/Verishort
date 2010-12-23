open Ast
open Imst
open Parser
open Asttoimst
open Str

module StringMap = Map.Make(String)

(* prepend identifiers with an underscore so that we never use an Verilog keyword. *)
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
	| ImRange(id, ind1, ind2) -> ((mod_id id) ^ (if ((ind1 = 0) && (ind2 = 0)) then "" else "[" ^ (string_of_int ind1) ^ ":" ^ (string_of_int ind2) ^ "]"))

let stringify_concat = function
	  ImConcatLit(replications,literal) -> raise (Failure("duh."))
	| ImConcatLvalue(replications, lv) -> string_of_int replications ^ "{" ^ (stringify_lvalue lv) ^ "}"

let stringify_concats lst = 
	let concat_string = List.map stringify_concat lst in

       "{" ^ (String.concat ", " concat_string) ^ "}"

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

let print_inst out inst_map (modname, bindlst1, bindlst2) = 
    let new_map = update_inst_count modname inst_map in
    let ind = StringMap.find modname new_map in
    output_string out ("_" ^ modname ^ " ___" ^ modname ^ (string_of_int ind) ^ "(");
    output_string out (String.concat ", " ((List.map (fun(id, exp) -> "._" ^ id ^ "(" ^ (stringify_expression exp) ^ ")") (bindlst1 @ bindlst2)) @ (
    print_if_necessary "clock" bindlst1) @ (print_if_necessary "reset" bindlst1)));
    output_string out ");\n";
    new_map
let rec print_case out (b, stmt) = output_string out ((string_of_int (String.length b)) ^ "'b" ^ b ^ ": \n"); output_string out "begin\n"; List.iter (print_statement out) stmt; output_string out "end\n"
and print_case_list out lst = List.iter (print_case out) lst
and print_statement out = function
	  ImNop -> ()
	| ImIf(pred,tru,fal) -> output_string out ("if (" ^ (stringify_expression pred) ^ ")\n"); output_string out "begin\n"; List.iter (print_statement out) tru; output_string out "end\nelse\nbegin\n"; List.iter (print_statement out) fal; output_string out "end\n"
	| ImCase(lv,csl) -> output_string out ("casex(" ^ (stringify_lvalue lv) ^ ")\n"); print_case_list out csl; output_string out "endcase\n"
	| ImRegAssign(lv, expr) -> output_string out ((stringify_lvalue lv) ^ "=" ^ (stringify_expression expr) ^ ";\n")

let print_module_sig out m = 
  (* print module sig *)
  let mod_args = (m.im_inputs, m.im_outputs) in
  let mod_arg_list (inputs, outputs) = 
  String.concat ", " (List.map (fun (name, _) -> "_" ^ name) (inputs @ outputs)) in
  (output_string out ("module _" ^ m.im_modname ^ "(" ^ (mod_arg_list mod_args) ^ ");\n");
  List.iter (fun (id, width ) -> output_string out ("input " ^ (if width == 1 then "" else ("[" ^ string_of_int (width - 1) ^ ":0] ")) ^ "_" ^ id ^ ";\n")) (fst mod_args);
  List.iter (fun (id, width ) -> output_string out ("output " ^ (if width == 1 then "" else ("[" ^ string_of_int (width - 1) ^ ":0] ")) ^ "_" ^ id ^ ";\n")) (snd mod_args))
  
  (* print decls *)
let print_decl out (typ, str, width) = output_string out ((if typ = ImWire then "wire" else "reg") ^ " " ^ (if width == 1 then "" else ("[" ^ (string_of_int (width - 1)) ^ ":0] ")) ^ "_" ^ str ^ ";\n")  

let print_assignment out (lv, expr) = output_string out ("assign " ^ (stringify_lvalue lv) ^ " = " ^ (stringify_expression expr) ^ ";\n")

(* print a standard library module *)
let print_libmod out libname libwidth actualname = 			
	let filename = (Filename.current_dir_name ^ "/stdlib/" ^ libname ^ ".v") in
	let chan = open_in filename in
	try
		while true; do
			let rawline = input_line chan in
			let replname = Str.global_replace (Str.regexp_string libname) actualname rawline in
			let replwidth = Str.global_replace (Str.regexp_string "WIDTHMINUSONE") (if libwidth = 1 then "" else ("[" ^ (string_of_int (libwidth - 1)) ^ ":0]")) replname in
			output_string out (replwidth  ^ "\n")
		done;
	with End_of_file -> close_in chan
	
let print_module out m =
	if m.im_libmod then print_libmod out m.im_libmod_name m.im_libmod_width m.im_modname else (
   print_module_sig out m;
   List.iter (print_decl out) m.im_declarations;
   List.iter (print_assignment out) m.im_assignments;
   ignore (List.fold_left (print_inst out) StringMap.empty m.im_instantiations);
   output_string out "always @ (*) begin\n";
   List.iter (print_statement out) m.im_alwaysall;
   output_string out "if (_reset) begin\n";
   List.iter (fun (typ, name, _) -> if typ = ImReg then output_string out ("_" ^ name ^ "= 0;") else ()) m.im_declarations;
   output_string out "end\nend\n";
   output_string out "always @ (posedge _clock) begin\n";
   List.iter (print_statement out) m.im_alwaysposedge;
   output_string out "end\n";
   output_string out "always @ (negedge _clock) begin\n";
   List.iter (print_statement out) m.im_alwaysnegedge;
   output_string out "end\n";
   output_string out "endmodule\n")
   
   
   
let _ =
  let inname = if Array.length Sys.argv > 1 then Sys.argv.(1) else "stdin" in
  let inchannel = if Array.length Sys.argv > 1 then Pervasives.open_in Sys.argv.(1) else stdin in
  let outchannel = if Array.length Sys.argv > 2 then Pervasives.open_out Sys.argv.(2) else stdout in
  let lexbuf = Lexing.from_channel inchannel in
  try 
	  let sourcecode = List.rev (Parser.program Scanner.token lexbuf) in
	    List.iter (print_module outchannel) (translate sourcecode)
  with Parse_Failure(msg, pos) -> print_endline (inname ^ ":" ^ (string_of_int pos.Lexing.pos_lnum) ^":" ^ (string_of_int (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)) ^": " ^ msg )
