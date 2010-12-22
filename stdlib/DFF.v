module DFF(_clock,_reset,_D,_S,_Q,_QNOT);
	input _clock;
	input _reset;
	input _D;
	input _S;
	output _Q;
	output _QNOT;
	
	reg Q;
	assign _Q = Q;
	assign _QNOT = ~Q;
	
	always @ (*) begin 
		if (_reset) begin
			Q = 0;
			end
		else if (_S) begin
			Q = 1;
			end
		end
	
	always @ (posedge _clock) begin
		if (~_reset & ~_S) begin
			Q = _D;
			end
		end

	endmodule
