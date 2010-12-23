module DFF(_clock,_reset,_D,_S,_Q,_QNOT, _return);
	input _clock;
	input _reset;
	input WIDTHMINUSONE _D;
	input WIDTHMINUSONE _S;
	output WIDTHMINUSONE _Q;
	output WIDTHMINUSONE _QNOT;
	output WIDTHMINUSONE _return;
	reg Q;
	assign _Q = Q;
	assign _QNOT = ~Q;
	assign _return = Q;	
	always @ (*) begin 
		if (_reset) begin
			Q = 0;
			end
		else
			Q = Q | _S;
		end
	
	always @ (posedge _clock) begin
	    if (_reset) begin
	        Q = 0;
	        end
		else begin
		     Q = (~_S & _D) | _S ;
		end
		end
	endmodule
