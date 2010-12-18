(* convert an AST to an IMST, with checking *)
open Ast
open Imst

module StringMap = Map.Make(String)

(* Environment information *)
type enviro = {
  local_map    : declaration list StringMap.t; (* FP offset for args, locals *)
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
  
  

let check_mod_info lst1 mods = 
  List.fold_left (fun lst mod1 -> if List.mem mod1.modname lst then raise (Parse_Failure("Duplicate module name." , mod1.modpos)) else if mod1.returnwidth < 0 then raise (Parse_Failure("Invalid return width.", mod1.modpos)) else mod1.modname :: lst) lst1 mods
  
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


let rec get_min_bit_width x = 
  if Int64.compare Int64.zero x = 0 || Int64.compare Int64.one x = 0 || Int64.compare Int64.minus_one x = 0 then 1
  else 1 + get_min_bit_width (Int64.div x (Int64.of_int 2))
  
let print_module env m = 

  (* print decls *)
  let print_decl decl = 
    output_string out_file ((if decl.decltype = Wire then "wire" else "reg") ^ " " ^ (if decl.declwidth == 1 then "" else ("[" ^ (string_of_int (decl.declwidth - 1)) ^ ":0] ")) ^ decl.declname ^ ";\n"); (match decl.init with
        Noexpr(_) -> ()
      | x -> 
      let value = eval_expr m.modname env x in 
      if get_min_bit_width value > decl.declwidth then raise (Parse_Failure("Overflow in initialization.", decl.declpos)) else output_string out_file ("assign " ^ decl.declname ^ " = " ^ Int64.to_string(value) ^ ";\n"))
  in
    List.iter print_decl m.declarations;\
  
  output_string out_file "endmodule\n"
  (** HERE **)
  
let rec get_arg_tuple name lst =
  match lst with
      [] -> raise Not_found
    | (s, i, _)::tl -> if s = name then (s,i) else get_arg_tuple name tl 

let get_arg mod_name output_name env = 
  let tuples = StringMap.find mod_name env.arg_map in
  get_arg_tuple arg_name (fst tuples @ snd tuples)

let get_output mod_name output_name env =
  let tuples = StringMap.find mod_name env.arg_map in
  get_arg_tuple arg_name (snd tuples)

let rec get_local_tuple name lst =
  match lst with
      [] -> raise Not_found
    | hd::tl -> if hd.declname = name then (hd.declname, hd.declwidth) else get_local_tuple name tl
in

let get_local mod_name local_name env = 
  get_local_tuple local_name (StringMap.find mod_name env.local_map)


let check_valid_lvalue environ mod_name lvalue_id pos = 
  (* arg map, local map *)
  try get_arg mod_name lvalue_id environ
  with Not_found -> try get_local mod_name lvalue_id environ with Not_found -> raise (Parse_Failure("Undefined identifier.", pos))
  
let check_assignment_lvalue environ mod_name lvalue_id pos = 
  (* arg map, local map *)
  try get_output mod_name lvalue_id environ
  with Not_found -> try get_local mod_name lvalue_id environ with Not_found -> raise (Parse_Failure("Undefined identifier.", pos))

let get_lvalue_min_length environ mod_name lvalue pos = match (to_im_lvalue environ immod lvalue pos) with
      ImIdentifier(_) -> (snd check_valid_lvalue environ mod_name lvalue pos)
  	| ImSubscript(_, _) -> 1
  	| ImRange(_, upper, lower) -> (upper - lower + 1)

let to_im_op = function
    Plus -> ImPlus  | Minus -> ImMinus  | Multiply -> ImMultiply | Divide -> ImDivide  
  | Modulus -> ImModulus  | Eq -> ImEq  | Ne -> ImNe | Ge -> ImGe  | Le -> ImLe  
  | Lt -> ImLt  | And -> ImAnd | Or -> ImOr | Xor -> ImXor | Nand -> ImNand 
  | Nor -> ImNor | Xnor -> ImXnor | Lshift -> ImLshift | Rshift -> ImRshift
  
let to_im_lvalue environ immod lval pos = match l with
   Identifier(i) ->  ImIdentifier(fst (check_valid_lvalue environ immod.im_modname i pos))
 | Subscript(s, expr) -> 
     let (id, width) = check_valid_lvalue environ immod.im_modname s pos in
     let subscr = Int64.to_int (eval_expr expr) in
     if subscr < 0 || subscr >= width then raise (Parse_Failure("Bus index out of bounds.", pos)) else ImSubscript(id, subscr)
 | Range(r, expr1, expr2) ->
     let (id, width) = check_valid_lvalue environ immod.im_modname s pos in
     let subscr1 = Int64.to_int (eval_expr expr1) in
     let subscr2 = Int64.to_int (eval_expr expr2) in
     if subscr1 < 0 || subscr2 < 0|| subscr1 >= width || subscr2 >= width then raise (Parse_Failure("Bus index out of bounds.", pos)) 
     else if subscr1 < subscr2 then raise (Parse_Failure("Bus ranges must be specified from most significant to least significant."))
     else ImRange( id, subscr1, subscr2)

let rec get_min_bit_width_expr environ immod expr = match expr with
    DLiteral(d, _) -> get_min_bit_width (Int64.of_int d)
  | BLiteral(b, _) -> String.length b
  | Lvalue(l, pos) -> get_lvalue_min_length environ immod.mod_name l pos
  | Binop(e1, op, e2, _) -> match op with
      Plus -> max (get_min_bit_width_expr e1) (get_min_bit_width_expr e2)
    | Minus -> max (get_min_bit_width_expr e1) (get_min_bit_width_expr e2)
    | Multiply -> (get_min_bit_width_expr e1) + (get_min_bit_width_expr e2) - 1
    | Modulus -> get_min_bit_width e2
    | Eq -> 1 | Ne -> 1 | Ge -> 1 | Gt -> 1 | Le -> 1 | Lt -> 1
    | And -> max (get_min_bit_width_expr e1) (get_min_bit_width_expr e2)
    | Or -> max (get_min_bit_width_expr e1) (get_min_bit_width_expr e2)
    | Xor -> max (get_min_bit_width_expr e1) (get_min_bit_width_expr e2)
    | Nand -> max (get_min_bit_width_expr e1) (get_min_bit_width_expr e2)
    | Nor -> max (get_min_bit_width_expr e1) (get_min_bit_width_expr e2)
    | Xnor -> max (get_min_bit_width_expr e1) (get_min_bit_width_expr e2)
    | Lshift -> get_min_bit_width_expr e1 | Rshift -> get_min_bit_width_expr e1
  | Assign(lvalue, expr, _) -> get_min_bit_width_expr expr
  | Signext(bits, expr, _) -> bits
  | Reduct(op, lvalue, _) -> 1
  | Not(expr, _) -> get_min_bit_width_expr expr
  | Concat(lst, pos) -> List.fold_left (fun orig x -> (match x with
  	   ConcatBLiteral(time, lit) -> orig + time * (String.length lit)
  	 | ConcatLvalue(time, lvalue) -> orig + time * (get_lvalue_min_length environ immod.mod_name lvalue pos))) 0 lst
  | Inst(str, bindlst1, bindlst2, pos) -> (try StringMap.find str env.return_map 
  	with Not_found -> raise (Parse_Failure("Undefined module name.", pos)))
  | Reset(_) -> 1
  | Noexpr(_) -> 0
  
let translate_expr environ immod expr = match expr with
    DLiteral(d, _) -> ImLiteral(Int64.of_int d, get_min_bit_width (Int64.of_int d) )
  | BLiteral(b, _) -> ImLiteral((try 
              if x.[0] = '0' then Int64.of_string ("0b" ^ x) 
              else Int64.neg (Int64.of_string ("0b"^(add_one (invert_binary x))))
              with Failure(_) -> raise (Parse_Failure("Binary literals may not exceed 64 bits", pos))), String.length b)
  | Lvalue(l, pos) -> ImLvalue(to_im_lavalue environ immod l pos)
  | Binop(e1, op, e2, _) -> ImBinop(translate_expr environ immod e1, to_im_op op, translate_expr environ immod e2)
  | Assign(lvalue, expr, _) -> (** **)
  | Signext(bits, expr, _) -> 
  | Reduct(op, lvalue, _) -> ImReduct(to_im_op op, to_im_lvalue lvalue)
  | Not(expr, _) -> ImNot(translate_expr environ immod expr)
  | Concat(lst, _) -> 
  | Inst(str, bindlst1, bindlst2, _) -> 
  | Reset(_) -> ImIdentifier("reset")
  | Noexpr(_) -> ImNoexpr
in   
let translate_if_always environ immod (expr, stmt1, stmt2) = 
  let imalways = {im_expr
in   
let translate_if environ immod (cond, stmt1, stmt2) = match cond
    with
    Posedge -> 
  | Negedge ->
  | Expression(expr) -> translate_if_always environ immod (expr, stmt1, stmt2)
in   

let rec translate_stmt environ immod vshstmt = match vshstmt
    with Nop -> []
  | Expr(expr, _) -> ignore (translate_expr environ immod expr); [] 
  | Block(lst, _) -> List.concat (List.map (translate_stmt environ immod) lst)
  | If(cond, stmt1, stmt2, _) -> translate_if environ immod (cond, stmt1, stmt2)
  | Case(lvalue, lst, _) ->
  | Return(expr, _) ->
  | For(init, cond, incr, stmt, _) -> 

(* translate_module: env -> mod_decl -> im_mod_decl*)
let translate_module environ vshmod = 
  let ret = { im_modname = vshmod.modname } in
  (* build up inputs and outputs *)
  let ret = { ret with im_inputs = List.map (fun (i, w, _) -> (i, w)) vshmod.inputs } in
  let ret = { ret with im_outputs = List.map (fun (i, w, _) -> ( i, w)) vshmod.outputs } in
  let ret = { ret with im_outputs = (if vshmod.returnwidth = 0 then ret.im_outputs else ("_return", vshmod.returnwidth) :: ret.im_outputs) } in
  (* build up initial declarations and initializations *)
  let to_im_decl_type = function Reg -> ImReg | Wire -> ImWire in
  let (decls, assigns) = List.fold_left (fun (olddecl, oldassign) decl -> ((to_im_decl_type decl.decltype), decl.declname, decl.declwidth), (ImIdentifier(decl.declname, (match decl.init with
   Noexpr(_) -> ImNoexpr
  | x -> let value = eval_expr vshmod.modname environ x in 
      if get_min_bit_width value > decl.declwidth then raise (Parse_Failure("Overflow in initialization.", decl.declpos)) else ImLiteral(value, decl.declwidth)))) ([], []) mod.declarations in
    let ret = { ret with im_declarations = decls; im_assignments = assigns } in

  List.iter(
  
  
    

(* translate: mod_decl list -> im_mod_decl list *)  

let translate modules =

  (* Check that the module names are consistent and the return widths are valid. *)
  let mod_names = check_mod_info [] modules in
  (* Build the environment *)
  let environment = { 
  arg_map  = string_map_args StringMap.empty modules; 
  param_map = string_map_params StringMap.empty modules;
  local_map = string_map_locals StringMap.empty modules;
  return_map = string_map_returns StringMap.empty modules;
  }  
  in check_unique_ids environment mod_names; List.map (translate_module env) modules
