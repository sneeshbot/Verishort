module SRL(_clock,_reset,_S,_E,_Q,_QNOT);
	input _clock;
	input _reset;
	input _S;
	input _E;
	output _Q;
	output _QNOT;
	
	reg Q;
	assign _Q = Q;
	assign _QNOT = ~Q;
	
	always @ (*) begin
		if (_E) begin 
			if (_reset) begin
				Q = 0;
				end
			else if (_S) begin
				Q = 1;
				end
			end
		end
	endmodule
