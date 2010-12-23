module TL(_clock,_reset,_T,_E,_Q,_QNOT, _return);
	input _clock;
	input _reset;
	input WIDTHMINUSONE _T;
	input WIDTHMINUSONE _E;
	output WIDTHMINUSONE _Q;
	output WIDTHMINUSONE _QNOT;
	output WIDTHMINUSONE _return;
	reg WIDTHMINUSONE Q;
	assign _Q = Q;
	assign _QNOT = ~Q;
	assign _return = Q;
	always @ (*) begin
		if (_reset) begin
			Q = 0;
			end
		else
			Q = (_E & _T & ~Q) | (~(_E & _T) & Q);
		end
		
	endmodule
