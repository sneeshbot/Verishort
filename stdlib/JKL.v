module JKL(_clock,_reset,_J,_K,_E,_Q,_QNOT, _return);
	input _clock;
	input _reset;
	input WIDTHMINUSONE _J;
	input WIDTHMINUSONE _K;
	input WIDTHMINUSONE _E;
	output WIDTHMINUSONE _Q;
	output WIDTHMINUSONE _QNOT;
	output WIDTHMINUSONE _return;
	reg WIDTHMINUSONE Q_reg;
	assign _Q = Q_reg;
	assign _QNOT = ~Q_reg;
	assign _return = Q_reg;
	always @ (*) begin
		if (_reset) begin 
			Q_reg = 0;
			end
		else begin
		    Q_reg = (_E & ( (_J & _K & ~Q_reg) | (_J & ~_K))) | (~_E & Q_reg) ;
		end 
		end
	endmodule
