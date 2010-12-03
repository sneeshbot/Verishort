open Ast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let sourcecode = Parser.sourcecode Scanner.token lexbuf in
  let print_param s =  
		print_string s; print_string ", " in
  let print_input s = 
		print_string "input "; print_string s; print_endline ";" in
  let print_output s = 
		print_string "output "; print_string s; print_endline ";" in
  let print_module mdecl = 
		print_string "module "; print_string mdecl.modname; print_string "("; 
List.iter print_param mdecl.inputs; 
List.iter print_param mdecl.outputs; print_endline "vs$return);"; 
List.iter print_input mdecl.inputs; 
List.iter print_output mdecl.outputs; print_endline "output vs$return;"; print_endline "endmodule" in
  List.iter print_module sourcecode
		
