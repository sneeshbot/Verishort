module DL(_clock,_reset,_D,_E,_Q,_QNOT);
	input _clock;
	input _reset;
	input _D;
	input _E;
	output _Q;
	output _QNOT;
	
	reg Q;
	assign _Q = Q;
	assign _QNOT = ~Q;
	
	always @ (*) begin 
		if (_reset) begin
			Q = 0;
			end
		else if (_E) begin
			if (_D) begin
				Q = 1;
				end
			else begin
				Q = 0;				
				end
			end
		end
	endmodule
