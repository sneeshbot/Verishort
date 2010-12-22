(* convert an AST to an IMST, with checking *)
open Ast
open Imst

module StringMap = Map.Make(String)

(* Environment information *)
type enviro = {
  local_map    : declaration list StringMap.t; 
  param_map    : parameter list StringMap.t;
  arg_map      : (id_with_width list * id_with_width list) StringMap.t;
  return_map   : int StringMap.t
}
  
let string_map_args map mods = 
  List.fold_left (fun m mod1 -> StringMap.add mod1.modname (mod1.inputs, mod1.outputs) m) map mods 

let string_map_params map mods = 
  List.fold_left (fun m mod1 -> StringMap.add mod1.modname mod1.parameters m) map mods

let string_map_locals map mods = 
  List.fold_left (fun m mod1 -> StringMap.add mod1.modname mod1.declarations m) map mods
  
let string_map_returns map mods = 
  List.fold_left (fun m mod1 -> StringMap.add mod1.modname mod1.returnwidth m) map mods
  
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
    
let rec invert_binary_actual n x = 
  if n < 0 then x else
  (x.[n] <- (if x.[n] = '1' then '0' else '1'); invert_binary_actual (n-1) x)
and invert_binary x = invert_binary_actual (String.length x - 1) (String.copy x)

let rec add_one_actual n x =
  if n < 0 then x (*discard overflow bit*)
  else
  if x.[n] = '1' then (x.[n] <- '0'; add_one_actual (n-1) x) else (x.[n] <- '1'; x)
and add_one x = add_one_actual (String.length x -1) (String.copy x)

let rec eval_expr mod_name env = function
    DLiteral(x, _) -> Int64.of_int x
  | BLiteral(x, pos) -> (try 
              if x.[0] = '0' then Int64.of_string ("0b" ^ x) 
              else Int64.neg (Int64.of_string ("0b"^(add_one (invert_binary x))))
              with Failure(_) -> raise (Parse_Failure("Binary literals may not exceed 64 bits", pos)))
  | Lvalue(x, pos) -> (match x with
        Identifier(id) -> (try Int64.of_int(get_param mod_name id env) 
                         with Not_found -> raise (Parse_Failure("Expression cannot be evaluated at compile time.", pos)))
      | _ -> raise (Parse_Failure("Expression cannot be evaluated at compile time.", pos)))
  | Binop(e1, op, e2, pos) -> (match op with
      Plus -> Int64.add (eval_expr mod_name env e1) (eval_expr mod_name env e2) 
    | Minus -> Int64.sub (eval_expr mod_name env e1) (eval_expr mod_name env e2)
    | Multiply -> Int64.mul (eval_expr mod_name env e1) (eval_expr mod_name env e2)
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
		| Not -> raise (Parse_Failure("Internal compiler error 003: contact manufacturer for assistance.", pos))
  )
  | Signext(_, _, pos) -> raise (Parse_Failure("Sign extension cannot be used in the current context.", pos))
  | Reduct(_, _, pos) -> raise (Parse_Failure("Expression cannot be evaluated at compile time.", pos))
  | Unary (op, exp, pos) -> (match op with
		| Not -> Int64.lognot (eval_expr mod_name env exp)
		| Plus -> (eval_expr mod_name env exp)
		| Minus -> Int64.neg (eval_expr mod_name env exp)
		| _ -> raise (Parse_Failure("Internal compiler error 002: contact manufacturer for assistance.", pos))
		)
  | Concat(_, pos) -> raise (Parse_Failure("Concatenation cannot be used in the current context.", pos))
  | Inst(_,_,_,pos) -> raise (Parse_Failure("Expression cannot be evaluated at compile time.", pos))
  | Reset(pos) -> raise (Parse_Failure("Expression cannot be evaluated at compile time.", pos))
  | Noexpr(pos) -> raise (Parse_Failure("Internal compiler error 001: contact manufacturer for assistance.", pos))
  

 
let get_arg mod_name arg_name env = 
  let tuples = StringMap.find mod_name env.arg_map in
  get_arg_tuple arg_name((fst tuples) @ (snd tuples) )
  

let check_mod_info lst1 mods = 
  List.fold_left (fun lst mod1 -> if List.mem mod1.modname lst then raise (Parse_Failure("Duplicate module name." , mod1.modpos)) else if mod1.returnwidth < 0 then raise (Parse_Failure("Invalid return width.", mod1.modpos)) else mod1.modname :: lst) lst1 mods
  
let check_unique_ids_in_module env mod1 =
	let mod_name = mod1.modname in
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


let rec get_min_bit_width x = 
  if Int64.compare Int64.zero x = 0 || Int64.compare Int64.one x = 0 || Int64.compare Int64.minus_one x = 0 then 1
  else 1 + get_min_bit_width (Int64.div x (Int64.of_int 2))
  
  
let rec get_arg_tuple name lst =
  match lst with
      [] -> raise Not_found
    | (s, i, _)::tl -> if s = name then (s,i) else get_arg_tuple name tl 

let get_arg mod_name arg_name env = 
  let tuples = StringMap.find mod_name env.arg_map in
  get_arg_tuple arg_name (fst tuples @ snd tuples)

let get_input mod_name input_name env =
  let tuples = StringMap.find mod_name env.arg_map in
  get_arg_tuple input_name (fst tuples)

let get_output mod_name output_name env =
  let tuples = StringMap.find mod_name env.arg_map in
  get_arg_tuple output_name (snd tuples)

let rec get_local_tuple name lst =
  match lst with
      [] -> raise Not_found
    | hd::tl -> if hd.declname = name then (hd.declname, hd.declwidth) else get_local_tuple name tl

let rec get_local_decl name lst =
  match lst with
      [] -> raise Not_found
    | hd::tl -> if hd.declname = name then hd else get_local_decl name tl

let get_local_all mod_name local_name env =
	get_local_decl local_name (StringMap.find mod_name env.local_map)

let get_local mod_name local_name env = 
  get_local_tuple local_name (StringMap.find mod_name env.local_map)

let get_lvalue_name = function
	Identifier(n) -> n | Subscript(n, _) -> n | Range(n, _, _) -> n

let change_im_lvalue_name newname = function
	ImSubscript(_, s) -> ImSubscript(newname, s) | ImRange(_, up, lo) -> ImRange(newname, up, lo)

let check_valid_lvalue environ mod_name lvalue_id pos = 
  (* arg map, local map *)
  try get_arg mod_name lvalue_id environ
  with Not_found -> try get_local mod_name lvalue_id environ with Not_found -> raise (Parse_Failure("Undefined identifier.", pos))
 
let check_assignment_lvalue environ mod_name lvalue_id pos = 
  (* arg map, local map *)
  try get_output mod_name lvalue_id environ
  with Not_found -> try get_local mod_name lvalue_id environ with Not_found -> raise (Parse_Failure("Undefined identifier.", pos))

let to_im_lvalue environ immod lval pos = match lval with
   Identifier(i) ->  ImRange(fst (check_valid_lvalue environ immod.im_modname i pos), snd (check_valid_lvalue environ immod.im_modname i pos) - 1, 0)
 | Subscript(s, expr) -> 
     let (id, width) = check_valid_lvalue environ immod.im_modname s pos in
     let subscr = Int64.to_int (eval_expr immod.im_modname environ expr) in
     if subscr < 0 || subscr >= width then raise (Parse_Failure("Bus index out of bounds.", pos)) else ImSubscript(id, subscr)
 | Range(r, expr1, expr2) ->
     let (id, width) = check_valid_lvalue environ immod.im_modname r pos in
     let subscr1 = Int64.to_int (eval_expr immod.im_modname environ expr1) in
     let subscr2 = Int64.to_int (eval_expr immod.im_modname environ expr2) in
     if subscr1 < 0 || subscr2 < 0|| subscr1 >= width || subscr2 >= width then raise (Parse_Failure("Bus index out of bounds.", pos)) 
     else if subscr1 < subscr2 then raise (Parse_Failure("Bus ranges must be specified from most significant to least significant.", pos))
     else ImRange(id, subscr1, subscr2)

let get_lvalue_length environ immod lvalue pos = match (to_im_lvalue environ immod lvalue pos) with
  	  ImSubscript(_, _) -> 1
  	| ImRange(_, upper, lower) -> (upper - lower + 1)

let rec check_im_mod_local_actual name = function
	 [] -> false
	| (_, declname, _) :: tl -> if name = declname then true else check_im_mod_local_actual name tl
and check_im_mod_local immod name = check_im_mod_local_actual name immod.im_declarations  

let get_lvalue_bit_range environ immod lvalue pos = match (to_im_lvalue environ immod lvalue pos) with
	    ImSubscript(_, s) -> (s, s)
  	| ImRange(_, upper, lower) -> (upper, lower)

let to_im_op = function
    Plus -> ImPlus  | Minus -> ImMinus  | Multiply -> ImMultiply | Gt -> ImGt
  | Modulus -> ImModulus  | Eq -> ImEq  | Ne -> ImNe | Ge -> ImGe  | Le -> ImLe  
  | Lt -> ImLt  | And -> ImAnd | Or -> ImOr | Xor -> ImXor | Nand -> ImNand 
  | Nor -> ImNor | Xnor -> ImXnor | Lshift -> ImLshift | Rshift -> ImRshift | Not -> ImNot
 

let rec get_max_bit_width_expr environ immod expr = match expr with
    DLiteral(d, _) -> 64
  | BLiteral(b, _) -> String.length b
  | Lvalue(l, pos) -> get_lvalue_length environ immod l pos
  | Binop(e1, op, e2, pos) -> (match op with
      Plus -> max (get_max_bit_width_expr environ immod  e1) (get_max_bit_width_expr environ immod e2)
    | Minus -> max (get_max_bit_width_expr environ immod  e1) (get_max_bit_width_expr environ immod e2)
    | Multiply -> (get_max_bit_width_expr environ immod  e1) + (get_max_bit_width_expr environ immod e2) - 1
    | Modulus -> get_max_bit_width_expr environ immod e2
    | Eq -> 1 | Ne -> 1 | Ge -> 1 | Gt -> 1 | Le -> 1 | Lt -> 1
    | And -> max (get_max_bit_width_expr environ immod e1) (get_max_bit_width_expr environ immod e2)
    | Or -> max (get_max_bit_width_expr environ immod e1) (get_max_bit_width_expr environ immod e2)
    | Xor -> max (get_max_bit_width_expr environ immod e1) (get_max_bit_width_expr environ immod e2)
    | Xnor -> max (get_max_bit_width_expr environ immod e1) (get_max_bit_width_expr environ immod e2)
    | Lshift -> get_max_bit_width_expr environ immod e1 | Rshift -> get_max_bit_width_expr environ immod e1
	| _ -> raise (Parse_Failure("Internal compiler error 003: contact manufacturer for assistance.", pos)))
  | Signext(bits, expr, _) -> bits
  | Reduct(op, lvalue, _) -> 1
  | Unary(_, expr, _) -> get_max_bit_width_expr environ immod expr
  | Concat(lst, pos) -> List.fold_left (fun orig x -> (match x with
  	   ConcatBLiteral(time, lit) -> orig + time * (String.length lit)
  	 | ConcatLvalue(time, lvalue) -> orig + time * (get_lvalue_length environ immod lvalue pos))) 0 lst
  | Inst(str, bindlst1, bindlst2, pos) -> (try StringMap.find str environ.return_map 
  	with Not_found -> raise (Parse_Failure("Undefined module name.", pos)))
  | Reset(_) -> 1
  | Noexpr(_) -> 0

let rec get_min_bit_width_expr environ immod expr = match expr with
    DLiteral(d, _) -> get_min_bit_width (Int64.of_int d)
  | BLiteral(b, _) -> String.length b
  | Lvalue(l, pos) -> get_lvalue_length environ immod l pos
  | Binop(e1, op, e2, pos) -> (match op with
      Plus -> max (get_min_bit_width_expr environ immod e1) (get_min_bit_width_expr environ immod e2)
    | Minus -> max (get_min_bit_width_expr environ immod e1) (get_min_bit_width_expr environ immod e2)
    | Multiply -> (get_min_bit_width_expr environ immod e1) + (get_min_bit_width_expr environ immod e2) - 1
    | Modulus -> get_min_bit_width_expr environ immod e2
    | Eq -> 1 | Ne -> 1 | Ge -> 1 | Gt -> 1 | Le -> 1 | Lt -> 1
    | And -> max (get_min_bit_width_expr environ immod e1) (get_min_bit_width_expr environ immod e2)
    | Or -> max (get_min_bit_width_expr environ immod e1) (get_min_bit_width_expr environ immod e2)
    | Xor -> max (get_min_bit_width_expr environ immod e1) (get_min_bit_width_expr environ immod e2)
    | Xnor -> max (get_min_bit_width_expr environ immod e1) (get_min_bit_width_expr environ immod e2)
    | Lshift -> get_min_bit_width_expr environ immod e1 | Rshift -> get_min_bit_width_expr environ immod e1
	| _ -> raise (Parse_Failure("Internal compiler error 003: contact manufacturer for assistance.", pos)))
  | Signext(bits, expr, _) -> bits
  | Reduct(op, lvalue, _) -> 1
  | Unary(_, expr, _) -> get_min_bit_width_expr environ immod expr
  | Concat(lst, pos) -> List.fold_left (fun orig x -> (match x with
  	   ConcatBLiteral(time, lit) -> orig + time * (String.length lit)
  	 | ConcatLvalue(time, lvalue) -> orig + time * (get_lvalue_length environ immod lvalue pos))) 0 lst
  | Inst(str, bindlst1, bindlst2, pos) -> (try StringMap.find str environ.return_map 
  	with Not_found -> raise (Parse_Failure("Undefined module name.", pos)))
  | Reset(_) -> 1
  | Noexpr(_) -> 0
 
(* translate_expr: environment -> im_moddecl -> expr -> bool -> im_moddecl * im_expr * int *)
(* Translate an AST expression into an IM expression *)
let rec translate_expr environ immod expr count in_conditional = match expr with
    DLiteral(d, _) -> (immod, ImLiteral(Int64.of_int d, get_min_bit_width (Int64.of_int d)), count)
  | BLiteral(b, pos) -> (immod, ImLiteral((try 
              if b.[0] = '0' then Int64.of_string ("0b" ^ b) 
              else Int64.neg (Int64.of_string ("0b"^(add_one (invert_binary b))))
              with Failure(_) -> raise (Parse_Failure("Binary literals may not exceed 64 bits", pos))), String.length b), count)
  | Lvalue(l, pos) -> (immod, ImLvalue(to_im_lvalue environ immod l pos), count)
  | Binop(e1, op, e2, _) -> let (immod1, imexp1, count1) = translate_expr environ immod e1 count in_conditional in
														let (immod2, imexp2, count2) = translate_expr environ immod1 e2 count1 in_conditional in
														(immod2, ImBinop(imexp1, to_im_op op, imexp2), count2)
  | Signext(bits, exp, pos) -> (* generate a temporary wire to store the value of expr. Then, use the concatentation syntax. *)
	 let width = get_min_bit_width_expr environ immod expr in
	 if width > bits then raise (Parse_Failure("Cannot sign extend something into fewer bits than the original.", pos)) else
	 let (immod1, imexpr1, count1) = translate_expr environ immod exp count in_conditional in 
	 ({ immod1 with im_declarations = (ImWire, "_im_" ^ (string_of_int count1), width) :: immod1.im_declarations;
	                im_assignments = (ImRange("_im_" ^ (string_of_int count1), width - 1, 0), imexpr1) :: immod1.im_assignments},
		ImConcat([ImConcatLvalue(bits - width, ImSubscript("_im_" ^ (string_of_int count1), width - 1));
		          ImConcatLvalue(1, ImRange("_im_" ^ (string_of_int count1), width - 1, 0))]), count + 1)
  | Reduct(op, lvalue, pos) -> (immod, ImReduct(to_im_op op, to_im_lvalue environ immod lvalue pos), count)
  | Unary(op, expr, _) -> let (immod1, imexp1, count1) = translate_expr environ immod expr count in_conditional in (immod1, ImUnary(to_im_op op, imexp1), count1)
  | Concat(lst, pos) -> let (immod1, concatlst1, count1) = List.fold_left (fun (immod2, lst2, count2) x -> match x with
		ConcatBLiteral(time, lit) -> if time <= 0 then raise (Parse_Failure("Replication must be at least one time.", pos)) else
		let immod3 = { immod2 with im_declarations = (ImWire, "_im_" ^ (string_of_int count2), String.length lit) :: immod2.im_declarations; im_assignments = (ImRange("_im_" ^ (string_of_int count2), String.length lit - 1, 0), ImLiteral((try 
              if lit.[0] = '0' then Int64.of_string ("0b" ^ lit) 
              else Int64.neg (Int64.of_string ("0b"^(add_one (invert_binary lit))))
              with Failure(_) -> raise (Parse_Failure("Binary literals may not exceed 64 bits", pos))), String.length lit)) :: immod2.im_assignments } in
         (immod3, ImConcatLvalue(time, ImRange("_im_" ^ (string_of_int count2), String.length lit - 1, 0)) :: lst2, count2 + 1)
	| ConcatLvalue(time, lvalue) -> if time <= 0 then raise (Parse_Failure("Replication must be at least one time.", pos)) else
	   (immod2, ImConcatLvalue(time, to_im_lvalue environ immod lvalue pos) :: lst2, count2)) (immod, [], count) lst in
	   (immod1, ImConcat(List.rev concatlst1), count1)
  | Reset(_) -> (immod, ImLvalue(ImRange("reset", 0, 0)), count)
  | Noexpr(_) -> (immod, ImNoexpr, count)
  | Inst(othermod, bindlst1, bindlst2, pos) -> if in_conditional then raise (Parse_Failure("Modules may not be instantiated inside conditional blocks.", pos)) else
		( 
		(* Check bindings *)
	  let (immod1, count1, converted_bindings_in) = convert_bindings_in environ count immod othermod bindlst1 pos in_conditional in
		let (immod2, count2, converted_bindings_out) = convert_bindings_out environ count immod1 othermod bindlst2 pos in
		try
			let returnwidth = StringMap.find othermod environ.return_map in
			if returnwidth = 0 then
				(* No return value - just do instantiation *) 
				( { immod2 with im_instantiations = (othermod, converted_bindings_in, converted_bindings_out) :: immod2.im_instantiations; }, ImNoexpr, count2)
			else
				(* return value - Do the following:*)
				(* Generate a bus for this purpose with the name _im_ followed by count, then increment count.. *)
				(* Add binding between "return" port and the new bus. *)
				let new_bus_name = "_imexp_" ^ (string_of_int count2) in 
			  let new_bindings_out = ("return", ImLvalue(ImRange(new_bus_name, returnwidth - 1, 0))) :: converted_bindings_out in
				( { immod2 with im_instantiations = (othermod, converted_bindings_in, new_bindings_out) :: immod2.im_instantiations;
				                im_declarations = (ImWire, new_bus_name, returnwidth) :: immod2.im_declarations; },
												ImLvalue(ImRange(new_bus_name, returnwidth-1, 0)), count2 + 1)
		with Not_found -> raise (Parse_Failure("Undefined module name.", pos))
		)	
and add_param_pos startpos endpos paramname lst pos = if startpos < endpos then lst 
          else let newparamplace = paramname ^ (string_of_int startpos) in 
               if List.mem newparamplace lst then raise (Parse_Failure("Duplicate binding.", pos))
               else add_param_pos (startpos - 1) endpos paramname (newparamplace::lst) pos
							
(* convert_bindings_in: env -> int -> im_moddecl -> string -> binding_in list -> im_moddecl * int * im_assignment list *)
(* Check: no duplicate bindings (but partial binding is OK, and do not have to bind anything).*)
(* Expr to be assigned to will be translated into ImExpr*)
(* The expr bound to can be anything. Do not check for uninitialized wires. *) 
and convert_bindings_in environ count immod othermod bindlst pos in_conditional =
	let (immod1, count1, list1, _) = (List.fold_left (fun (mod1, cnt1, bnd1, lst1) (name, exp1) -> (let (_, width) = get_input othermod name environ in
		                      let lst2 = add_param_pos (width - 1) 0 name lst1 pos in
													if width < get_min_bit_width_expr environ immod exp1 then raise (Parse_Failure("Binding width mismatch.", pos))
													else if width > get_max_bit_width_expr environ immod exp1 then raise (Parse_Failure("Binding width mismatch.", pos))
													else let (mod2, exp2, cnt2) = translate_expr environ mod1 exp1 cnt1 in_conditional in
													(mod2, cnt2, (name, exp2) :: bnd1, lst2)))
	  (immod, count, [], []) bindlst) in (immod1, count1, list1)

(* convert_bindings_out: env -> im_moddecl -> string -> binding_out list -> im_moddecl * int * im_assignment list *)
(* Check: no duplicate bindings to ports (but partial binding is OK, and do not have to bind anything). *)
(* Note that duplicate assignments to wires will result in undefined behavior. *)
(* Check that the target of the assignment is a wire - "binding" a reg to an output does not make any sense. *)
and convert_bindings_out environ count immod othermod bindlst pos =
	let (immod1, count1, list1 , _) = (List.fold_left (fun (mod1, cnt1, bnd1, lst1) (name, lval2) -> 
		try (
		(try
			let result = get_local_all immod.im_modname (get_lvalue_name lval2) environ in
		  if result.decltype = Reg then raise (Parse_Failure("Cannot bind output port to registers.", pos)) else ()
		with Not_found -> ignore (get_output immod.im_modname (get_lvalue_name lval2) environ)); 
		let exp1 = Lvalue(lval2, Lexing.dummy_pos) in
			let (_, width) = get_output othermod name environ in
		                      let lst2 = add_param_pos (width - 1) 0 name lst1 pos in
													if width <> get_min_bit_width_expr environ immod exp1 then raise (Parse_Failure("Binding width mismatch.", pos))
													else let (mod2, exp2, cnt2) = translate_expr environ mod1 exp1 cnt1 false in
													(mod2, cnt2, (name, exp2) :: bnd1, lst2))
		with Not_found -> raise (Parse_Failure("Undefined identifier.", pos)))
	  (immod, count, [], []) bindlst) in (immod1, count1, list1)


(* translate_stmt: enviro -> im_moddecl -> stmt -> int -> bool -> im_moddecl * im_always_stmt list * int *)
(* If in_always is false and we are not currently in an always block (that is, if/case), *)
(* or, in other words, we are in a top-level statement, then statement *)
(* generated is returned through modification to the im_moddecl directly. *)
(* Otherwise, it is returned in the list for incorporation by a top-level statement *)
let rec translate_stmt environ immod vshstmt count in_always = match vshstmt
    with Nop(_) -> (immod, [], count)
  | Expr(expr, _) -> let (immod1, _, count1) = translate_expr environ immod expr count in_always in (immod1, [], count1) 
  | Block(lst, _) -> List.fold_left (fun (immod1, stmtlst1, count1) stmt -> 
		                    let (immod2, stmtlst2, count2) = translate_stmt environ immod1 stmt count1 in_always in
											  (immod2, stmtlst1 @ stmtlst2, count2)) (immod, [], count) lst
  | If(cond, stmt1, stmt2, pos) -> if in_always && (cond = Posedge || cond = Negedge) then raise (Parse_Failure("Clock edge conditions must be in the outermost if statement", pos))
		                                else (match cond with
																			Posedge -> let (immod1, stmtlist1, count1) = translate_stmt environ immod stmt1 count true in
																								 ( { immod1 with im_alwaysposedge = immod1.im_alwaysposedge @ stmtlist1 }, [], count1 )
																		| Negedge -> let (immod1, stmtlist1, count1) = translate_stmt environ immod stmt1 count true in
																								 ( { immod1 with im_alwaysnegedge = immod1.im_alwaysnegedge @ stmtlist1 }, [], count1 ) 
																		| Expression(expr) -> if in_always then 
																			(  let (immod1, stmtlist1, count1) = translate_stmt environ immod stmt1 count true in
																			   let (immod2, stmtlist2, count2) = translate_stmt environ immod1 stmt2 count1 true in
																			   let (immod3, imexpr, count3) = translate_expr environ immod2 expr count true in
																			   (immod3, [ImIf(imexpr, stmtlist1, stmtlist2)], count3))
																			else
																			   let (immod1, stmtlist1, count1) = translate_stmt environ immod stmt1 count true in
																			   let (immod2, stmtlist2, count2) = translate_stmt environ immod1 stmt2 count1 true in
																			   let (immod3, imexpr, count3) = translate_expr environ immod2 expr count false in
																			   ( {immod3 with im_alwaysall = ImIf(imexpr, stmtlist1, stmtlist2) :: immod3.im_alwaysall}, [], count3)
																		)
  | Return(expr, pos) -> translate_stmt environ immod (Assign(Identifier("return"), expr, pos)) count in_always
	| Case(lvalue, lst, pos) -> let width = get_max_bit_width_expr environ immod (Lvalue(lvalue, pos)) in
														let (newmod, newcount, newlist) = List.fold_left (fun (immod1, count1, lst1) (item, stmt1, pos) -> 
															if String.length item <> width then raise (Parse_Failure("Width mismatch in case statement.", pos))
															else let (immod2, stmtlist2, count2) = translate_stmt environ immod1 stmt1 count1 true in
															(immod2, count2, (item, stmtlist2) :: lst1)) (immod, count, []) lst in
														if in_always then
															(newmod, [ImCase(to_im_lvalue environ newmod lvalue pos, List.rev newlist)], newcount)
														else
															({newmod with im_alwaysall = ImCase(to_im_lvalue environ newmod lvalue pos, List.rev newlist) :: newmod.im_alwaysall}, [], newcount)
  | For(id, init, cond, incr, stmt, pos) -> 
		(* How this works: We add the loop variable as a parameter in the local parameter table, then translate the statement.*)
		(* Rinse, repeat. Loop for a maximum of 1024 times. *)
		(* First, make sure that id is not referring to anything else. *)
		(try
		let _ = get_param immod.im_modname id environ in raise (Parse_Failure("Loop control variable must not have been used previously.", pos))
		with Not_found -> (try let _ = get_arg immod.im_modname id environ in raise (Parse_Failure("Loop control variable must not have been used previously.", pos))
		with Not_found -> (try let _ = get_local immod.im_modname id environ in raise (Parse_Failure("Loop control variable must not have been used previously.", pos))
		with Not_found -> (
			(* compute the initialization value *)
			let firstval = Int64.to_int (eval_expr immod.im_modname environ init) in
			(* build_for: int -> int -> im_moddecl -> int -> im_moddecl * im_always_stmt list * int *)
			let rec build_for currval loopsleft mod1 count = (if loopsleft < 0 then raise (Parse_Failure("For loop has run too many times.", pos)) else
				(*Add currval to local parameter table*)
				let newenv = {environ with param_map = StringMap.add mod1.im_modname ((id, currval, Lexing.dummy_pos) :: (StringMap.find mod1.im_modname environ.param_map)) environ.param_map } in
				let continue = eval_expr mod1.im_modname newenv cond in
				if Int64.compare Int64.zero continue = 0 then (mod1, [], count)
				else
					let (mod2, stmtlist2, count2) = translate_stmt newenv mod1 stmt count in_always in
					let newval = Int64.to_int (eval_expr mod1.im_modname newenv incr) in
					let (mod3, stmtlist3, count3) = build_for newval (loopsleft - 1) mod2 count2 in
					(mod3, stmtlist2 @ stmtlist3, count3)
				)
			in
			let (mod4, stmtlist, newcount) = build_for firstval 1024 immod count in
			if in_always then (mod4, stmtlist, newcount)
			else ( { mod4 with im_alwaysall = List.rev_append stmtlist mod4.im_alwaysall}, [], newcount) 
		)))) 
	| Assign(lvalue, expr, pos) -> (
		let imlvalue = to_im_lvalue environ immod lvalue pos in 
		let lvalue_width = get_lvalue_length environ immod lvalue pos in
		let lvaluename = get_lvalue_name lvalue in
		if lvalue_width < get_min_bit_width_expr environ immod expr then raise (Parse_Failure("Assignment width mismatch.", pos))
		else if lvalue_width > get_max_bit_width_expr environ immod expr then raise (Parse_Failure("Assignment width mismatch.", pos))
		else if in_always then (
			try
				let decl = get_local_all immod.im_modname lvaluename environ in
				if decl.decltype = Reg then 
				let (immod1, imexpr, count1) = translate_expr environ immod expr count true in
				(immod1, [ImRegAssign(imlvalue, imexpr)], count1)
				else 
				let tempregname = "_reg_" ^ lvaluename in
				if check_im_mod_local immod tempregname then
				  let (immod1, imexpr, count1) = translate_expr environ immod expr count true in
					  (immod1, [ImRegAssign(change_im_lvalue_name tempregname imlvalue, imexpr)], count1)
				else
					let immod1 = { immod with im_declarations = (ImReg, tempregname, decl.declwidth) :: immod.im_declarations;
																	  im_assignments = (ImRange(lvaluename, decl.declwidth - 1, 0), ImLvalue(ImRange(tempregname, decl.declwidth - 1, 0))) :: immod.im_assignments } in
					let (immod2, imexpr, count1) = translate_expr environ immod1 expr count true in
					  (immod2, [ImRegAssign(change_im_lvalue_name tempregname imlvalue, imexpr)], count1)
						
			with Not_found -> let tempregname = "_reg_" ^ lvaluename in
			  let (_, width) = get_output immod.im_modname lvaluename environ in
				if check_im_mod_local immod tempregname then
				  let (immod1, imexpr, count1) = translate_expr environ immod expr count true in
					  (immod1, [ImRegAssign(change_im_lvalue_name tempregname imlvalue, imexpr)], count1)
				else
					let immod1 = { immod with im_declarations = (ImReg, tempregname, width) :: immod.im_declarations;
                                    im_assignments = (ImRange(lvaluename, width - 1, 0), ImLvalue(ImRange(tempregname, width - 1, 0))) :: immod.im_assignments } in
					let (immod2, imexpr, count1) = translate_expr environ immod1 expr count true in
					  (immod2, [ImRegAssign(change_im_lvalue_name tempregname imlvalue, imexpr)], count1)
			)
		else (
			try
				let decl = get_local_all immod.im_modname lvaluename environ in
				if decl.decltype = Reg then raise (Parse_Failure("Cannot assign values to registers outside if and case blocks. Use wires.", pos))
				else let (immod1, imexpr, count1) = translate_expr environ immod expr count false in
				if imexpr = ImNoexpr then raise (Parse_Failure("Invalid right hand side value in assignment.", pos))
				else ({immod1 with im_assignments = (imlvalue, imexpr) :: immod1.im_assignments}, [], count1)
			with Not_found ->  let (immod1, imexpr, count1) = translate_expr environ immod expr count false in
				if imexpr = ImNoexpr then raise (Parse_Failure("Invalid right hand side value in assignment.", pos))
				else ({immod1 with im_assignments = (imlvalue, imexpr) :: immod1.im_assignments}, [], count1)
			)
		)

let rec check_assignment_duplication startpos endpos paramname lst pos = if startpos < endpos then lst 
          else let newparamplace = paramname ^ (string_of_int startpos) in 
               if List.mem newparamplace lst then raise (Parse_Failure("Duplicate assignment of \"" ^ paramname ^ "["^ (string_of_int startpos) ^ "]\".", pos))
               else add_param_pos (startpos - 1) endpos paramname (newparamplace::lst) pos
							
(* translate_module: env -> mod_decl -> im_mod_decl*)
let translate_module environ vshmod = 
	if vshmod.libmod then { im_modname = vshmod.modname; im_libmod = true; im_libmod_name = vshmod.libmod_name;
	im_libmod_width = vshmod.libmod_width; im_inputs = []; im_outputs = []; im_declarations = [];	im_assignments = [];
	im_instantiations = [];	im_alwaysall=[];	im_alwaysposedge = [];	im_alwaysnegedge = []; }
	else (
	ignore (check_unique_ids_in_module environ vshmod); (* check that all identifiers used in the module are unique *)
  let ret = { im_modname = vshmod.modname; im_libmod = false; im_libmod_name = ""; 	im_libmod_width = 0;	im_inputs = [];
	im_outputs = [];	im_declarations = [];	im_assignments = [];	im_instantiations = [];	im_alwaysall=[];
	im_alwaysposedge = [];im_alwaysnegedge = [];} in
  (* build up inputs and outputs *)
  let ret = { ret with im_inputs = List.map (fun (i, w, _) -> (i, w)) vshmod.inputs } in
  let ret = { ret with im_outputs = List.map (fun (i, w, _) -> ( i, w)) vshmod.outputs } in
	(* special output for returns. Note that "return" is never a valid name because it is a keyword! *)
  let ret = { ret with im_outputs = (if vshmod.returnwidth = 0 then ret.im_outputs else ("return", vshmod.returnwidth) :: ret.im_outputs) } in
  (* Build up initial declarations and initializations from the ones provided.
	   Initializations *must* be to an expression evaluable at compile time.
		 This constraint and the parameter - declaration - statements sequence allows us to not worry about scope. *)
  let to_im_decl_type = function Reg -> ImReg | Wire -> ImWire in
  let (decls, assigns) = List.fold_left (fun (olddecl, oldassign) decl -> 
	 (
		((to_im_decl_type decl.decltype), decl.declname, decl.declwidth) :: olddecl,
	  (match decl.init with
			   Noexpr(_) -> oldassign  
	     | x -> (ImRange(decl.declname, decl.declwidth - 1, 0), (let value = eval_expr vshmod.modname environ x in 
			          if get_min_bit_width value > decl.declwidth then 
									raise (Parse_Failure("Overflow in initialization.", decl.declpos)) 
								else ImLiteral(value, decl.declwidth))) :: oldassign
	 ))) ([], []) vshmod.declarations in
    let ret = { ret with im_declarations = decls; im_assignments = assigns } in
		let (immod, _, _) = 
			List.fold_left (fun (immod1, _, count) stmt -> translate_stmt environ immod1 stmt count false) (ret, [], 0) vshmod.statements in
		let finalmod = { immod with im_alwaysall = List.rev immod.im_alwaysall; im_instantiations = List.rev immod.im_instantiations; im_assignments = List.rev immod.im_assignments } in
		 ignore (List.fold_left (fun lst1 lval1 -> (match lval1 with
		  ImSubscript(name, s) -> check_assignment_duplication s s name lst1 vshmod.modpos
		| ImRange(name, up, lo) -> check_assignment_duplication up lo name lst1 vshmod.modpos
		)) [] ((List.map (fun (s, _) -> s) finalmod.im_assignments) @ 
		List.map (fun (_, exp) -> match exp with ImLvalue(l) -> l 
		           | _ -> raise (Parse_Failure("Internal compiler error 005. Contact manufacturer for more information.", vshmod.modpos))) 
			(List.flatten (List.map (fun (_, _, l) -> l) finalmod.im_instantiations)))); finalmod
  )
    
let set_standard_library_module_info mod1 = if mod1.libmod then (
	if mod1.libmod_width < 1 then raise (Parse_Failure("Invalid standard module width.", mod1.modpos))
	else match mod1.libmod_name with
	  "SRL" -> {mod1 with inputs = [("clock", 1, Lexing.dummy_pos); ("reset", 1, Lexing.dummy_pos); 
                                  ("S", mod1.libmod_width, Lexing.dummy_pos); ("E", mod1.libmod_width, Lexing.dummy_pos)];
												outputs = [("Q", mod1.libmod_width, Lexing.dummy_pos);("QNOT", mod1.libmod_width, Lexing.dummy_pos)];
												returnwidth = mod1.libmod_width; } 
  | "JKL" -> {mod1 with inputs = [("clock", 1, Lexing.dummy_pos); ("reset", 1, Lexing.dummy_pos); 
                                  ("J", mod1.libmod_width, Lexing.dummy_pos); ("K", mod1.libmod_width, Lexing.dummy_pos);
																	("E", mod1.libmod_width, Lexing.dummy_pos)];
												outputs = [("Q", mod1.libmod_width, Lexing.dummy_pos);("QNOT", mod1.libmod_width, Lexing.dummy_pos)];
												returnwidth = mod1.libmod_width; }
	| "DL"  -> {mod1 with inputs = [("clock", 1, Lexing.dummy_pos); ("reset", 1, Lexing.dummy_pos); 
                                  ("D", mod1.libmod_width, Lexing.dummy_pos); ("E", mod1.libmod_width, Lexing.dummy_pos)];
												outputs = [("Q", mod1.libmod_width, Lexing.dummy_pos);("QNOT", mod1.libmod_width, Lexing.dummy_pos)];
												returnwidth = mod1.libmod_width; }
	| "TL"  -> {mod1 with inputs = [("clock", 1, Lexing.dummy_pos); ("reset", 1, Lexing.dummy_pos); 
                                  ("T", mod1.libmod_width, Lexing.dummy_pos); ("E", mod1.libmod_width, Lexing.dummy_pos)];
												outputs = [("Q", mod1.libmod_width, Lexing.dummy_pos);("QNOT", mod1.libmod_width, Lexing.dummy_pos)];
												returnwidth = mod1.libmod_width; }
	| "DFF" -> {mod1 with inputs = [("clock", 1, Lexing.dummy_pos); ("reset", 1, Lexing.dummy_pos); 
                                  ("D", mod1.libmod_width, Lexing.dummy_pos); ("S", mod1.libmod_width, Lexing.dummy_pos)];
												outputs = [("Q", mod1.libmod_width, Lexing.dummy_pos);("QNOT", mod1.libmod_width, Lexing.dummy_pos)];
												returnwidth = mod1.libmod_width; }
	| "TFF" -> {mod1 with inputs = [("clock", 1, Lexing.dummy_pos); ("reset", 1, Lexing.dummy_pos); 
                                  ("T", mod1.libmod_width, Lexing.dummy_pos)];
												outputs = [("Q", mod1.libmod_width, Lexing.dummy_pos);("QNOT", mod1.libmod_width, Lexing.dummy_pos)];
												returnwidth = mod1.libmod_width; }
	| "JKFF" -> {mod1 with inputs = [("clock", 1, Lexing.dummy_pos); ("reset", 1, Lexing.dummy_pos); 
                                  ("J", mod1.libmod_width, Lexing.dummy_pos); ("K", mod1.libmod_width, Lexing.dummy_pos)];
												outputs = [("Q", mod1.libmod_width, Lexing.dummy_pos);("QNOT", mod1.libmod_width, Lexing.dummy_pos)];
												returnwidth = mod1.libmod_width; }
	| "MUX" -> {mod1 with inputs = [("clock", 1, Lexing.dummy_pos); ("reset", 1, Lexing.dummy_pos); 
                                  ("IN", mod1.libmod_width, Lexing.dummy_pos); ("SEL", get_min_bit_width (Int64.of_int mod1.libmod_width), Lexing.dummy_pos)];
												outputs = [("OUT", 1, Lexing.dummy_pos)];
												returnwidth = 1; }
	| "DEMUX" -> {mod1 with inputs = [("clock", 1, Lexing.dummy_pos); ("reset", 1, Lexing.dummy_pos); 
                                  ("IN", 1, Lexing.dummy_pos); ("SEL", get_min_bit_width (Int64.of_int mod1.libmod_width), Lexing.dummy_pos)];
												outputs = [("OUT", mod1.libmod_width, Lexing.dummy_pos)];
												returnwidth = mod1.libmod_width; }
	| "DECODE" -> {mod1 with inputs = [("clock", 1, Lexing.dummy_pos); ("reset", 1, Lexing.dummy_pos); 
                                  ("IN", get_min_bit_width (Int64.of_int mod1.libmod_width), Lexing.dummy_pos)];
												outputs = [("OUT", mod1.libmod_width, Lexing.dummy_pos)];
												returnwidth = mod1.libmod_width; }
	| "ENCODE" -> {mod1 with inputs = [("clock", 1, Lexing.dummy_pos); ("reset", 1, Lexing.dummy_pos); 
                                  ("IN", mod1.libmod_width, Lexing.dummy_pos)];
												outputs = [("OUT", get_min_bit_width (Int64.of_int mod1.libmod_width), Lexing.dummy_pos)];
												returnwidth = get_min_bit_width (Int64.of_int mod1.libmod_width); }
	| _ -> raise (Parse_Failure("Unsupported standard module name.", mod1.modpos)))
	 else mod1
(* translate: mod_decl list -> im_mod_decl list *)  

let translate modules =

  (* Check that the module names are consistent and the return widths are valid. *)
  let _ = check_mod_info [] modules in
	(* Set information for standard library module declarations *)
	let modules = List.map set_standard_library_module_info modules in
  (* Build the environment *)
  let environment = { 
  arg_map  = string_map_args StringMap.empty modules; 
  param_map = string_map_params StringMap.empty modules;
  local_map = string_map_locals StringMap.empty modules;
  return_map = string_map_returns StringMap.empty modules;
  }  
  in List.map (translate_module environment) modules
