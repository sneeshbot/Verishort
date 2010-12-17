open Ast

module StringMap = Map.Make(String)

(* Symbol table: Information about all the names in scope *)
type enviro = {
    local_map    : declaration list StringMap.t; (* FP offset for args, locals *)
	param_map    : parameter list StringMap.t;
	arg_map      : (id_with_width list * id_with_width list) StringMap.t;
  }

let string_map_args map mods = 
  List.fold_left (fun m mod1 -> StringMap.add mod1.modname (mod1.inputs, mod1.outputs) m) map mods 

let string_map_params map mods = 
  List.fold_left (fun m mod1 -> StringMap.add mod1.modname mod1.parameters m) map mods

let string_map_locals map mods = 
  List.fold_left (fun m mod1 -> StringMap.add mod1.modname mod1.declarations m) map mods
  
let local_exist mod_name local_name env = 
  try List.mem local_name (StringMap.find mod_name env.local_map) with Not_found -> false
    
let get_param mod_name param_name env = 
  List.find param_name (StringMap.find mod_name env.param_map)

let rec get_arg_tuple name lst =
  match lst with
      [] -> raise Not_found
    | (s, i, _)::tl -> if s = name then (s,i) else get_arg_tuple name tl  
  
let get_arg mod_name arg_name env = 
  let tuples = StringMap.find mod_name env.arg_map in
  get_arg_tuple arg_name((fst tuples) @ (snd tuples) )
  
  

let check_modnames lst1 mods = 
  List.fold_left (fun lst mod1 -> if List.mem mod1.modname lst then raise (Parse_Failure("Duplicate module name." , mod1.modpos)) else mod1.modname :: lst) lst1 mods
let check_unique_ids_in_module env mod_name =
	let inputslist = List.fold_left (fun lst2 (name, width, pos)  -> (* each input *)
      if List.mem name lst2 
      then raise (Parse_Failure ("Duplicate identifier.", pos)) 
      else if width < 1 
      then raise (Parse_Failure ("Invalid width.", pos))
      else name :: lst2) [] (fst (StringMap.find mod_name env.arg_map)) 
    in let argslist = List.fold_left (fun lst2 (name, width, pos)  -> (* each output *)
      if List.mem name lst2 
      then raise (Parse_Failure ("Duplicate identifier.", pos)) 
      else if width < 1 
      then raise (Parse_Failure ("Invalid width.", pos))
      else name :: lst2) inputslist (snd (StringMap.find mod_name env.arg_map)) 
    in let argsandparamslist = List.fold_left (fun lst2 (name, _, pos)  ->
      if List.mem name lst2 
      then raise (Parse_Failure ("Duplicate identifier.", pos)) 
      else name :: lst2) argslist (StringMap.find mod_name env.param_map)  
    in 
    List.fold_left (fun lst2 decl  -> (* each decl in each mod *)
      if List.mem decl.declname lst2 
      then raise (Parse_Failure ("Duplicate identifier.", decl.declpos)) 
      else if decl.declwidth < 1 
      then raise (Parse_Failure ("Invalid width.", decl.declpos))
      else decl.declname :: lst2) argsandparamslist (StringMap.find mod_name env.local_map) 
      
let check_unique_ids env mod_names =
  List.iter (fun mod_name-> ignore(check_unique_ids_in_module env mod_name) ) mod_names

let translate modules =
	
	let mod_names = check_modnames [] modules in
	
	let environment = { arg_map	= string_map_args StringMap.empty modules; 

	param_map = string_map_params StringMap.empty modules;

	local_map = string_map_locals StringMap.empty modules
	}  in
	
	check_unique_ids environment mod_names
	
	

	 
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


	
