module(clock,reset,a,b);
	input clock;
	input reset;
	input a;
	output [9:0]b;
	assign b = a;
	endmodule
