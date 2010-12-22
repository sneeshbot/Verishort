module TL(_clock,_reset,_T,_E,_Q,_QNOT);
	input _clock;
	input _reset;
	input _T;
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
		else if (_E & _T) begin
			Q = ~Q;
			end
		end
	endmodule
