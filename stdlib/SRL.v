module SRL(clock,reset,S,E,Q,QNOT);
	input clock;
	input reset;
	input S;
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
			Q_reg = S;
			QNOT_reg = ~S;
			end
		end
	endmodule
