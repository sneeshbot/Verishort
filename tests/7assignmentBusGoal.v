module mod(clock,reset,a,b);
	input clock;
	input reset;

	input [11:0] a;
	output [11:0] b;
	assign b=a;
	endmodule
