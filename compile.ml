open Ast
open Big_int

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

let get_param_value (_, x, _) = x

let rec get_param_tuple name lst = 
  match lst with
      [] -> raise Not_found
    | (s, i, p)::tl -> if s = name then (s, i, p) else get_param_tuple name tl

let get_param mod_name param_name env = 
  get_param_value (get_param_tuple param_name (StringMap.find mod_name env.param_map))

let rec get_arg_tuple name lst =
  match lst with
      [] -> raise Not_found
    | (s, i, _)::tl -> if s = name then (s,i) else get_arg_tuple name tl
(*
let rec eval_expr mod_name env = function
    DLiteral(x, _) -> x
  | BLiteral(x, _) -> int_of_string ("0b" ^ x)
  | Lvalue(x, pos) -> (match x with
  	  Identifier(id) -> try get_param mod_name id env with Not_found -> raise (Parse_Failure("Expression cannot be evaluated at compile time.", pos))
  	| _ -> raise (Parse_Failure("Expression cannot be evaluated at compile time.", pos))
  	)
  | Binop(e1, op, e2) -> (match op with
	  	Plus -> eval_expr e1 + eval_expr e2 
	  | Minus -> eval_expr e1 - eval_expr e2
	  | Multiply -> eval_expr e1 * eval_expr e2 
	  | Divide -> eval_expr e1 / eval_expr e2 
	  | Modulus -> eval_expr e1 mod eval_expr e2  
	  | Eq -> if eval_expr e1 = eval_expr e2 then 1 else 0
	  | Ne -> if eval_expr e1 <> eval_expr e2 then 1 else 0 
	  | Ge -> if eval_expr e1 >= eval_expr e2 then 1 else 0
	  | Gt -> if eval_expr e1 > eval_expr e2 then 1 else 0
	  | Le -> if eval_expr e1 <= eval_expr e2 then 1 else 0
	  | Lt -> if eval_expr e1 < eval_expr e2 then 1 else 0
	  | And -> land (eval_expr e1) (eval_expr e2)
	  | Or -> lor (eval_expr e1) (eval_expr e2) 
	  | Xor -> lxor (eval_expr e1) (eval_expr e2)
	  | Nand -> lnot (land (eval_expr e1) (eval_expr e2))
	  | Nor -> lnot (lor (eval_expr e1) (eval_expr e2))
	  | Xnor -> lnot (lxor (eval_expr e1) (eval_expr e2))
	  | Lshift -> lsl (eval_expr e1) (eval_expr e2)
	  | Rshift -> asr (eval_expr e1) (eval_expr e2) (*arithmetic shift - keep sign*)
	)
  | Assign(_, _, pos) -> raise (Parse_Failure("Assignment cannot be used in the current context.", pos))
  | Signext(_, _, pos) -> raise (Parse_Failure("Sign extension cannot be used in the current context.", pos))
  | Reduct of op * expr * Lexing.position
  | Not (exp, _) -> lnot (eval_expr exp)
  | Concat(_, pos) -> raise (Parse_Failure("Concatenation cannot be used in the current context.", pos))
  | Inst(_,_,_,pos) -> raise (Parse_Failure("Expression cannot be evaluated at compile time.", pos))
  | Reset(pos) -> raise (Parse_Failure("Expression cannot be evaluated at compile time.", pos))
  | Noexpr -> raise (Parse_Failure("Internal compiler error 001: contact manufacturer for assistance.", pos))
*)


let rec eval_expr mod_name env = function
    DLiteral(x, _) -> Int64.of_int x
  | BLiteral(x, _) -> Int64.of_string ("0b" ^ x)
  | Lvalue(x, pos) -> (match x with
  	    Identifier(id) -> (try Int64.of_int(get_param mod_name id env) 
  	                      with Not_found -> raise (Parse_Failure("Expression cannot be evaluated at compile time.", pos)))
  	  (*| _ -> raise (Parse_Failure("Expression cannot be evaluated at compile time.", pos))*)
  	  | Range(_,_,_) -> raise (Parse_Failure("Expression cannot be evaluated at compile time.", pos))
      | Subscript(_,_) -> raise (Parse_Failure("Expression cannot be evaluated at compile time.", pos))
  	  (*| (_,_,pos) -> raise (Parse_Failure("Expression cannot be evaluated at compile time.", pos))*)
  	  (*| (_,_,_,pos) -> raise (Parse_Failure("Expression cannot be evaluated at compile time.", pos))*)
  	)
  | Binop(e1, op, e2, _) -> (match op with
	  	Plus -> Int64.add (eval_expr mod_name env e1) (eval_expr mod_name env e2) 
	  | Minus -> Int64.sub (eval_expr mod_name env e1) (eval_expr mod_name env e2)
	  | Multiply -> Int64.mul (eval_expr mod_name env e1) (eval_expr mod_name env e2) 
	  | Divide -> Int64.div (eval_expr mod_name env e1) (eval_expr mod_name env e2) 
	  | Modulus -> Int64.rem (eval_expr mod_name env e1) (eval_expr mod_name env e2)  
	  | Eq -> if (Int64.compare (eval_expr mod_name env e1) (eval_expr mod_name env e2)) = 0 then Int64.one else Int64.zero
	  | Ne -> if (Int64.compare (eval_expr mod_name env e1) (eval_expr mod_name env e2)) <> 0 then Int64.one else Int64.zero
	  | Ge -> if (Int64.compare (eval_expr mod_name env e1) (eval_expr mod_name env e2)) >= 0 then Int64.one else Int64.zero
	  | Gt -> if (Int64.compare (eval_expr mod_name env e1) (eval_expr mod_name env e2)) > 0 then Int64.one else Int64.zero
	  | Le -> if (Int64.compare (eval_expr mod_name env e1) (eval_expr mod_name env e2)) <= 0 then Int64.one else Int64.zero
	  | Lt -> if (Int64.compare (eval_expr mod_name env e1) (eval_expr mod_name env e2)) < 0 then Int64.one else Int64.zero
	  | And -> Int64.logand (eval_expr mod_name env e1) (eval_expr mod_name env e2)
	  | Or -> Int64.logor (eval_expr mod_name env e1) (eval_expr mod_name env e2) 
	  | Xor -> Int64.logxor (eval_expr mod_name env e1) (eval_expr mod_name env e2)
	  | Nand -> Int64.lognot (Int64.logand (eval_expr mod_name env e1) (eval_expr mod_name env e2))
	  | Nor -> Int64.lognot (Int64.logor (eval_expr mod_name env e1) (eval_expr mod_name env e2))
	  | Xnor -> Int64.lognot (Int64.logxor (eval_expr mod_name env e1) (eval_expr mod_name env e2))
	  | Lshift -> Int64.shift_left (eval_expr mod_name env e1) (Int64.to_int (eval_expr mod_name env e2))
	  | Rshift -> Int64.shift_right (eval_expr mod_name env e1) (Int64.to_int (eval_expr mod_name env e2)) (*arithmetic shift - keep sign*)
	)
  | Assign(_, _, pos) -> raise (Parse_Failure("Assignment cannot be used in the current context.", pos))
  | Signext(_, _, pos) -> raise (Parse_Failure("Sign extension cannot be used in the current context.", pos))
  | Reduct(_, _, pos) -> raise (Parse_Failure("Expression cannot be evaluated at compile time.", pos))
  | Not (exp, _) -> Int64.lognot (eval_expr mod_name env exp)
  | Concat(_, pos) -> raise (Parse_Failure("Concatenation cannot be used in the current context.", pos))
  | Inst(_,_,_,pos) -> raise (Parse_Failure("Expression cannot be evaluated at compile time.", pos))
  | Reset(pos) -> raise (Parse_Failure("Expression cannot be evaluated at compile time.", pos))
  | Noexpr(pos) -> raise (Parse_Failure("Internal compiler error 001: contact manufacturer for assistance.", pos))
  

 
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

	let out_file = Pervasives.open_out("output_file") in
	
	let mod_names = check_modnames [] modules in
	
	let environment = { arg_map	= string_map_args StringMap.empty modules; 

	param_map = string_map_params StringMap.empty modules;

	local_map = string_map_locals StringMap.empty modules
	}  in
	
	check_unique_ids environment mod_names;
	
	Pervasives.close_out(out_file)


	
