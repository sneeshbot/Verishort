(*type statement = 
	Block of statement list
	| _

type module_declaration = {
	modname : string; (* Name of the module *)
	inputs : string list;
	outputs : string list;
	body : statement list;
	
	}*)
