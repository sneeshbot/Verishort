open Ast

module StringMap = Map.Make(String)

(* Symbol table: Information about all the names in scope *)
type env = {
    local_map    : declaration list StringMap.t; (* FP offset for args, locals *)
	param_map    : parameter list StringMap.t;
	arg_map     : (id_with_width list * id_with_width list) StringMap.t;
  }

let string_map_args map mods = 
  List.fold_left (fun m mod1 -> StringMap.add mod1.modname (mod1.inputs, mod1.outputs) m) map mods 

let string_map_params map mods = 
  List.fold_left (fun m mod1 -> StringMap.add mod1.modname mod1.parameters m) map mods

let string_map_locals map mods = 
  List.fold_left (fun m mod1 -> StringMap.add mod1.modname mod1.declarations m) map mods

let check_modnames lst1 mods = 
  List.fold_left (fun lst mod1 -> if List.mem mod1.modname lst then raise (Parse_Failure("Duplicate module name." , mod1.modpos)) else mod1.modname :: lst) lst1 mods

let translate modules =
	
	let mod_names = check_modnames [] modules in
	
	let environment = { arg_map	= string_map_args StringMap.empty modules; 

	param_map = string_map_params StringMap.empty modules;

	local_map = string_map_locals StringMap.empty modules
	}  in
	()
	
	

	 
	 (*
	let args_map	= string_map_args StringMap.empty modules in 

	let params_map = string_map_params StringMap.empty modules in 

	let locals_map = string_map_locals StringMap.empty modules in 

	let print_decl_alt x y = List.iter print_func y in
		
	let print_ids lst = List.iter (fun (id, width) -> (print_string (id^ "["); print_int width; print_endline "]")) lst in
	
	let print_params x lst =  List.iter (fun (a, b)  -> print_string (a ^ "="); print_endline (string_of_int b)) lst in
	
	StringMap.iter print_decl_alt locals_map;
	print_endline "**********";
	StringMap.iter (fun x (inputs, outputs) -> (print_endline ("Modname: " ^ x); print_endline "Inputs:"; print_ids inputs; print_endline "Outputs:"; print_ids outputs)) args_map;
	print_endline "**********";
	ignore (StringMap.iter print_params params_map)
	*)


	
