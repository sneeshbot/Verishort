module DL(_clock,_reset,_D,_E,_Q,_QNOT, _return);
	input _clock;
	input _reset;
	input WIDTHMINUSONE _D;
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
		else begin
		    Q = (_E & D) | (~_E & Q);
		end
		end
	endmodule
