type mod_decl= {
	modname : string; (* Name of the module *)
	inputs : string list;
	outputs : string list;
	(*body : statement list;*)
	
	}

type sourcecode = mod_decl list

