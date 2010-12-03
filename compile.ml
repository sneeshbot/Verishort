open Ast

module StringMap = Map.Make(String)

(* Symbol table: Information about all the names in scope *)
type env = {
    local_map    : declaration list StringMap.t; (* FP offset for args, locals *)
	param_map    : parameter list StringMap.t;
	args_map     : (id_with_width list * id_with_width list) StringMap.t;
  }

let string_map_args map mods = 
  List.fold_left (fun m mod1 name -> StringMap.add name (mod1.inputs, mod1.outputs) m) map mods 

let string_map_params map mods = 
  List.fold_left (fun m mod1 name -> StringMap.add name mod1.parameters m) map mods

let string_map_locals map mods = 
  List.fold_left (fun m mod1 name -> StringMap.add name mod1.declarations m) map mods

(** Translate a program in AST form into a bytecode program.  Throw an
    exception if something is wrong, e.g., a reference to an unknown
    variable or function *)
let translate modules =
	 
	let locals_map	= string_map_args StringMap.empty modules in 

	let params_map = string_map_params StringMap.empty modules in 

	let locals_map = string_map_locals StringMap.empty modules in 

	
