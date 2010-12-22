module JKL(_clock,_reset,_J,_K,_E,_Q,_QNOT);
	input _clock;
	input _reset;
	input _J;
	input _K;
	input _E;
	output _Q;
	output _QNOT;
	
	reg Q_reg;
	assign _Q = Q_reg;
	assign _QNOT = ~Q_reg;
	
	always @ (*) begin
		if (_reset) begin 
			Q_reg = 0;
			end
		else if (_E) begin
			if (_J & _K) begin
				Q_reg = ~Q_reg;
				end
		  	else if (_J) begin
		  		Q_reg = 1;
		  		end
			else if (_K) begin
				Q_reg = 0;
				end
			end
		end
	endmodule
