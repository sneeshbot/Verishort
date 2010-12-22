module JKL(clock,reset,J,K,E,Q,QNOT);
	input clock;
	input reset;
	input J;
	input K;
	input E;
	output Q;
	output QNOT;
	
	reg Q_reg;
	reg QNOT_reg;
	assign Q = Q_reg;
	assign QNOT = QNOT_reg;
	
	always @ (posedge clock) begin
		if (reset) begin 
			Q_reg = 0;
			QNOT_reg = 1;
			end
		else if (E) begin
			if (J & K) begin
				Q_reg = ~Q_reg;
				QNOT_reg = ~QNOT_reg;
				end
		  	else if (J) begin
		  		Q_reg = 1;
		  		QNOT_reg = 0;
		  		end
			else if (K) begin
				Q_reg = 0;
				QNOT_reg = 1;
				end
			end
		end
	endmodule
