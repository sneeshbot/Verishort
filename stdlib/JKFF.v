module JKFF(_clock,_reset,_J,_K,_E,_Q,_QNOT, _return);
	input _clock;
	input _reset;
	input [WIDTHMINUSONE:0] _J;
	input [WIDTHMINUSONE:0] _K;
	input [WIDTHMINUSONE:0] _E;
	output [WIDTHMINUSONE:0] _Q;
	output [WIDTHMINUSONE:0] _QNOT;
	output [WIDTHMINUSONE:0] _return;
	reg [WIDTHMINUSONE:0] Q_reg;
	assign _Q = Q_reg;
	assign _QNOT = ~Q_reg;
	assign _return = Q_reg;
	
	always @ (posedge _clock) begin
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
