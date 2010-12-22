module DL(clock,reset,D,E,Q,QNOT);
	input clock;
	input reset;
	input D;
	input E;
	output Q;
	output QNOT;
	
	reg Q_reg;
	reg QNOT_reg;
	assign Q = Q_reg;
	assign QNOT = QNOT_reg;
	
	always @ (posedge clk)
